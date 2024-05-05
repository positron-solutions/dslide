;;; macro-slides.el --- A presentation framework -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2011-2023 Takaaki ISHIKAWA
;; Copyright (C) 2024 Positron
;;
;; Author: Positron <contact@positron.solutions>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.2"))
;; Maintainer: Positron <contact@positron.solutions>
;; URL: https://github.com/positron-solutions/macro-slides
;; Keywords: convenience, org-mode, presentation, narrowing
;;
;; Committers: Takaaki ISHIKAWA <takaxp at ieee dot org>
;;             Yuuki ARISAWA (@uk-ar)
;;             Eric S Fraga
;;             Eike Kettner
;;             Stefano BENNATI
;;             Matus Goljer
;;             Boruch Baum
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Macro Slides is a highly programmable presentation framework that first
;; displays Org files as presentations but also can integrate your presentation
;; with any Emacs buffer and also with Org Babel.  By integrating arbitrary
;; Emacs Lisp into the simple forward-backward user interface, you can make
;; anything Emacs does easy to present.
;;
;; See the README and manual M-x info-display-manual RET macro-slides RET.
;;
;; There are examples of using the features within the test directory.
;;
;; Requirement:
;;    org-mode 9.6.x or higher version
;;    The latest version of the org-mode is recommended.
;;                      (see https://orgmode.org/)
;;
;; Usage:
;;    1. Open an org-mode file
;;    2. Run `ms-start'
;;
;; Note:
;;    - Customize variables, M-x customize-group RET macro-slides RET
;;
;; The code outline is as follows:
;;
;; 1. Lifecycle of the mode, switching between base buffer, contents, and
;; slides, user interface commands.
;;
;; 2. Class interface definitions for stateful sequence, deck (root sequence),
;; slide, and actions (sequences that run within slides).
;;
;; 3. Element mapping implementations that are private but exposed publicly on
;; slide actions and elsewhere because they are super useful.
;;
;; 4. Miscellaneous implementation details of parsing arguments, debug printing,
;; header, animation etc.
;;
;; For users, you might want to create your own actions, so check `ms-action'
;; and its sub-classes.
;;
;; The `ms-deck' class contains some functions related to adding callbacks or
;; entering custom sequences.
;;
;; For hackers wishing to extend the code, in addition, you will want to check
;; `ms--make-slide' if you want your slides to hydrate actions differently.
;; Also pay very close attention to `ms-stateful-sequence' and how sequences and
;; steps can be pushed, like a function call stack.
;;
;; This package is a fork and mostly complete re-write of org-tree-slide by
;; Takaaki ISHIKAWA.  Thanks to everyone who worked on org-tree-slide over the
;; years.  The implementation ideas and features of org-tree-slide were a great
;; inspiration for this package.

;;; Code:

(require 'image-mode)
(require 'eieio)
(require 'org-element)
(require 'org-fold)
(require 'face-remap)

(eval-when-compile (require 'cl-lib))

(defgroup macro-slides nil
  "User variables for `macro-slides'."
  :group 'outlines)

(defcustom ms-base-follows-slide t
  "Non-nil moves the base buffer point to the current slide.
This happens whether the buffer is visible or not."
  :type 'boolean
  :group 'macro-slides)

(defcustom ms-start-from 'first
  "When starting, begin at `point' `first' slide.
Any other value is equivalent to `first'.

If the contents are shown first, the point will be on the
configured slide.

This only has effect when starting the mode or commands that
implicitly start the mode.

- `first': Always begin the slideshow from the very first slide.

- `point': the slideshow always begins at the slide under point.

You can achieve `first' behavior by calling `ms-first-slide'.  If
you want to navigate slides with the point, you should use the
contents mode with `ms-contents'.  To avoid losing your place,
use `ms-slides' to toggle between the base buffer and slides
buffer."
  :type '(choice (const :tag "First slide" first)
                 (const :tag "Slide at point" point))
  :group 'macro-slides)

(defcustom ms-start-function #'ms-display-slides
  "When starting the mode, this is the default starting function.
It should usually call `ms-display-slides' or
`ms-display-contents'.  You can build commands that
use `let' binding to temporarily set this variable in order to
start with a specific starting function."
  :type 'function
  :group 'macro-slides)

(defcustom ms-header t
  "The status of displaying the slide header."
  :type 'boolean
  :group 'macro-slides)

(defcustom ms-contents-header t
  "Display header in contents buffer.
When this is disabled, the keywords for title etc will remain
visible, albeit scrolled away because of how `org-overview'
works."
  :type 'boolean
  :group 'macro-slides)

(defcustom ms-header-author t
  "Show the email in the header.
If there is a #+author: header, it will be used."
  :type 'boolean
  :group 'macro-slides)

(defcustom ms-header-email t
  "Show the email in the header.
If there is a #+email: header, it will be used."
  :type 'boolean
  :group 'macro-slides)

(defcustom ms-header-date t
  "Show the date in the header.
If there is a #+date: header, it will be used.
The current time will be used as a fallback."
  :type 'boolean
  :group 'macro-slides)

;; TODO make this number and support partial lines
(defcustom ms-content-margin-top 2
  "Specify the margin between the slide header and its content."
  :type 'integer
  :group 'macro-slides)

(defcustom ms-slide-in-effect t
  "Using a visual effect of slide-in for displaying trees."
  :type 'boolean
  :group 'macro-slides)

;; TODO support partial lines
(defcustom ms-slide-in-blank-lines 15
  "Line height of the slide-in effect."
  :type 'integer
  :group 'macro-slides)

(defcustom ms-feedback-messages
  '(:start "Start! ▶"
           :forward "Forward ➡"
           :backward "⬅ Backward"
           :contents "Contents ☰"
           :stop "Finished! ■"          ; TODO stop is not finish
           :after-last-slide "No more slides")
  "Feedback messages for slide controls.
Turn off by setting to nil.  Plist keys:
- :start `ms-start'
- :forward `ms-forward'
- :backward `ms-backward'
- :contents `ms-contents'
- :stop `ms-stop'"
  :type 'plist
  :group 'macro-slides)

(defcustom ms-breadcrumb-face nil
  "Face added to the list of faces for breadcrumbs.
This can be a face name symbol or an anonymous font spec.  It
will be added to the face list, meaning it the original face's
properties remain unless shadowed."
  :type 'face
  :group 'macro-slides)

(defface ms-heading-level-1 '((t :inherit 'org-level-1))
  "Org heading override."
  :group 'macro-slides)

(defface ms-heading-level-2 '((t :inherit 'org-level-2))
  "Org heading override."
  :group 'macro-slides)

(defface ms-heading-level-3 '((t :inherit 'org-level-3))
  "Org heading override."
  :group 'macro-slides)

(defface ms-heading-level-4 '((t :inherit 'org-level-4))
  "Org heading override."
  :group 'macro-slides)

(defface ms-heading-level-5 '((t :inherit 'org-level-5))
  "Org heading override."
  :group 'macro-slides)

(defface ms-heading-level-6 '((t :inherit 'org-level-6))
  "Org heading override."
  :group 'macro-slides)

(defface ms-heading-level-7 '((t :inherit 'org-level-7))
  "Org heading override."
  :group 'macro-slides)

(defface ms-heading-level-8 '((t :inherit 'org-level-8))
  "Org heading override."
  :group 'macro-slides)

(defface ms-document-title '((t :inherit 'org-document-title))
  "Org document title override."
  :group 'macro-slides)

(defface ms-document-info '((t :inherit 'org-document-info))
  "Org document info override."
  :group 'macro-slides)

(defface ms-header-overlay-face '((t :inherit default))
  "Face for `ms--header-overlay'."
  :group 'macro-slides)

(defcustom ms-breadcrumb-separator " 🢒 "
  "Delimiter for breadcrumbs or nil to turn off breadcrumbs."
  :type '(choice (const :tag "Don't display breadcrumbs" nil)
                 (string :tag "Delimiter"))
  :group 'macro-slides)

(defcustom ms-breadcrumbs-hide-todo-state t
  "If non-nil, hide TODO states in the breadcrumbs."
  :type 'boolean
  :group 'macro-slides)

(defcustom ms-start-hook nil
  "Runs after the slide buffer is created but before first slide.
Buffer is widened and fully visible."
  :group 'macro-slides
  :type 'hook)

(defcustom ms-stop-hook nil
  "Runs in the base buffer after stopping."
  :group 'macro-slides
  :type 'hook)

(defcustom ms-slide-hook nil
  "Runs after a slide has been switched to.
This includes the first slide of start, when switching back from
contents.  It is run after any slide actions."
  :group 'macro-slides
  :type 'hook)

(defcustom ms-narrow-hook nil
  "Runs whenever the slide buffer restriction is updated.
Use this hook for behaviors that affect the displayed region.
Slides and sequences that do not display themselves or only
affect display in another buffer will not trigger this hook."
  :group 'macro-slides
  :type 'hook)

(defcustom ms-contents-hook nil
  "Runs last after switching to contents."
  :group 'macro-slides
  :type 'hook)

(defcustom ms-after-last-slide-hook '(ms-stop)
  "Run when forward is called at last slide."
  :group 'macro-slides
  :type 'hook)

(defcustom ms-default-slide-action
  #'ms-action-narrow
  "Action class with lifecycle around the section actions.
When stepping forward or backward, it is called before any
section action.  It's normal purpose is to update the buffer
restriction before section-actions are run.

You can configure this per-heading by setting the
SLIDE_ACTION keyword.  You can configure it for
the document default by adding an SLIDE_ACTION
keyword."
  :type 'function
  :group 'macro-slides)

(defcustom ms-default-section-actions
  '()
  "Actions that run within the section display action lifecycle.
It's value is a list of `ms-action' sub-classes or (CLASS . ARGS)
forms where ARGS is a plist.  Each subclass will be instantiated
into an action object.  See the `ms-action' class and its methods
to learn about writing custom actions.

Many section actions are no-op whenever the content doesn't
contain any elements they act on.  You can add classes to this
list in order to have default behaviors for some org elements.

You can configure this per-heading by setting the
SLIDE_SECTION_ACTIONS keyword.  You can configure it for the
document default by adding an SLIDE_SECTION_ACTIONS keyword."
  :type '(list function)
  :group 'macro-slides)

(defcustom ms-default-child-action
  #'ms-child-action-slide
  "Action run after section lifecycle.
Value is an action class, usually extending
`ms-child-action'.  The usual purpose is to manage
the child headings, which come after the section element.

You can configure this per-heading by setting the
SLIDE_CHILD_ACTION keyword.  You can configure it for the
document default by adding an SLIDE_CHILD_ACTION keyword."
  :type 'function
  :group 'macro-slides)

(defcustom ms-default-class 'ms-slide
  "A class to more deeply modify slide behavior.
Value should be a custom class extending `ms'.  You
can override methods if the built-in implementation is
insufficient.  Consider upstreaming changes.

You can configure this per heading by setting the SLIDE_CLASS
property.  You can configure it for the document default by
adding an SLIDE_CLASS keyword."
  :type 'symbol
  :group 'macro-slides)

(defcustom ms-default-deck-class 'ms-deck
  "A class to more deeply modify overall deck behavior.
Value should be a custom class extending `ms-deck'.
Use this to modify the root-level behaviors, including switching
to children and finding siblings.  You can configure this for the
document by adding the SLIDE_ROOT_CLASS keyword."
  :type 'symbol
  :group 'macro-slides)

(defcustom ms-default-filter
  #'ms-built-in-filter
  "A function used to call next on children.
The function used as actions should accept an org element, a
`headline' type element and return it if it is a valid heading or
return nil if it should be skipped.

You can configure this per heading by setting the SLIDE_FILTER
keyword.  You can configure it for the document default by adding
an SLIDE_FILTER keyword."
  :type 'function
  :group 'macro-slides)

(defvar ms--debug nil
  "Set to t for logging slides and actions.")

;; Tell the compiler that these variables exist
(defvar ms-mode)

(defvar-local ms-heading-level-1-cookie nil)
(defvar-local ms-heading-level-2-cookie nil)
(defvar-local ms-heading-level-3-cookie nil)
(defvar-local ms-heading-level-4-cookie nil)
(defvar-local ms-heading-level-5-cookie nil)
(defvar-local ms-heading-level-6-cookie nil)
(defvar-local ms-heading-level-7-cookie nil)
(defvar-local ms-heading-level-8-cookie nil)
(defvar-local ms-document-title-cookie nil)
(defvar-local ms-document-info-cookie nil)

(defvar ms--deck nil
  "Active deck object.
This is global.  If a presentation is active, you can look at this variable to
coordinate with it.")

(defvar-local ms--overlays nil
  "Overlays used to hide or change contents display.")

(defvar-local ms--header-overlay nil
  "Flag to check the status of overlay for a slide header.")

;; * Lifecycle

(defvar-keymap ms-mode-map
  :doc "The keymap for `ms' mode."
  "<left>" #'ms-backward
  "<right>" #'ms-forward
  "<up>" #'ms-contents
  "<down>" #'ms-start)      ; TODO start is really toggle

;;;###autoload
(define-minor-mode ms-mode
  "A presentation tool for Org Mode."
  :init-value nil
  :keymap ms-mode-map
  :group 'macro-slides
  :global t
  (unless (eq 'org-mode (buffer-local-value
                         'major-mode (current-buffer)))
    (user-error "Not an org buffer")
    (ms-mode -1))
  (cond (ms-mode
         ;; Create the indirect buffer and link it via the deck object.
         (ms--ensure-deck)
         (funcall (or ms-start-function
                      #'ms-display-slides))
         (run-hooks 'ms-start-hook))
        (t
         (ms-stop))))

(defun ms-live-p ()
  "Check if a deck is associated so that commands can complete."
  (and ms-mode
       ms--deck
       (ms-deck-live-p ms--deck)))

;; TODO rename these functions to `switch-to'?
(defun ms-display-slides ()
  (ms--ensure-slide-buffer t)
  (ms--clean-up-state)
  (oset ms--deck slide-buffer-state 'slides)
  (widen)
  (org-fold-show-all)
  (ms-init ms--deck))

(defun ms-display-contents ()
  "Switch to showing contents in the slide buffer.
This is a valid `ms-start-function' and will start
each slide show from the contents view."
  (ms--ensure-slide-buffer t)
  (ms--clean-up-state)
  (oset ms--deck slide-buffer-state 'contents)

  (widen)
  (org-overview)

  (when ms-contents-header
    (if-let ((first (ms--document-first-heading)))
        (narrow-to-region (org-element-begin first)
                          (point-max))
      ;; No first heading.  Just header.  Empty contents.
      (narrow-to-region (point-max)
                        (point-max)))
    (ms--make-header t))

  ;; TODO walk all headings with the filter and add overlays on the hidden stuff
  ;; TODO filter slides that don't have a display action?

  (ms--feedback :contents)
  (run-hooks 'ms-contents-hook))

(defun ms-display-base ()
  "Switch to the base buffer for the slide show."
  (unless ms--deck
    (error "No deck exists"))
  (oset ms--deck slide-buffer-state 'base)
  (switch-to-buffer (oref ms--deck base-buffer)))

(defun ms-stop ()
  "Stop the presentation entirely.
Kills the indirect buffer, forgets the deck, and return to the
source buffer."
  (interactive)
  (when-let* ((deck ms--deck)
              (slide-buffer (oref deck slide-buffer))
              (base-buffer (oref deck base-buffer)))

    ;; TODO possibly finalize in state cleanup.  Slides <-> contents switching
    ;; may require attention.
    (ms-final ms--deck)

    ;; Animation timers especially should be stopped
    ;; TODO ensure cleanup is thorough even if there's a lot of failures.
    ;; TODO make the deck a child sequence of a presentation ;-)
    (ms--clean-up-state)

    (switch-to-buffer base-buffer)

    (when slide-buffer
      (kill-buffer slide-buffer))

    (when ms-mode
      (ms-mode -1))

    (setq ms--deck nil)

    (run-hooks 'ms-stop-hook)
    (ms--feedback :stop)))

;; * User Commands

;;;###autoload
(defun ms-contents ()
  "Toggle between slides and contents.
This command will activate the mode if it is inactive and show
the contents.  When the contents is shown, it will toggle back to
the slides.

This generic command should always toggle to some higher level
view where the user can move around a presentation sequence more
quickly."
  (interactive)
  (if (ms-live-p)
      (if (ms--showing-slides-p)
          (ms-display-contents)
        (ms-display-slides))
    (let ((ms-start-function
           #'ms-contents))
      (ms-mode 1))))

;;;###autoload
(defun ms-start ()
  "Go back to the slides or base buffer.
This command goes from the overview to the slides, from the
slides to the base buffer, or if no mode is active, will start
the mode and go to slides."
  (interactive)
  (if (ms-live-p)
      (if (ms--showing-slides-p)
          (ms-display-base)
        (ms-display-slides))
    (let ((ms-start-function
           #'ms-display-slides))
      (ms-mode 1))))

;; TODO forward and backward commands are usually only bound in the mode and
;;shouldn't need to check for the deck being active
;;;###autoload
(defun ms-forward ()
  "Advance slideshow forward."
  (interactive)
  (unless (ms-live-p)
    (user-error "No deck is active"))
  (if (ms--showing-contents-p)
      (org-next-visible-heading 1)
    (ms--ensure-slide-buffer)
    (ms-step-forward ms--deck)))

;;;###autoload
(defun ms-backward ()
  "Advance slideshow backward."
  (interactive)
  (unless (ms-live-p)
    (user-error "No deck is active"))
  (if (ms--showing-contents-p)
      (org-previous-visible-heading 1)
    (ms--ensure-slide-buffer)
    (ms-step-backward ms--deck)))

;; * Classes

(defclass ms-progress-tracking ()
  ((marker :initform nil :initarg :marker))
  "A utility class for other classes that track progress.
Progress is tracked by reading and updating a marker.")

(cl-defgeneric ms-marker (obj))

(cl-defmethod ms-marker ((obj ms-progress-tracking)
                         &optional pom)
  "Set internal marker to POM or return marker position if set.
Errors when asked for a marker before one has been set."
  (let ((marker (or (oref obj marker)
                    (pcase (type-of pom)
                      ('marker pom)
                      ('integer (set-marker (make-marker) pom))
                      ('symbol nil)))))
    (when (and marker pom)
      (set-marker marker pom))
    (if (and marker (marker-buffer marker))
        (marker-position (oset obj marker marker))
      (error "No marker was initialized"))))

;; This is one of the most important interfaces for all hacking.  The domain
;; model is that of a linear sequence of steps that the user traverses both
;; forward and backward.
;;
;; There are some states that may need to be set up or torn down at the
;; boundaries of the sequence.  These are handled by three methods, init, end,
;; and final.
;;
;; Sub-sequences currently don't have any special support for setup or teardown
;; when entering or exiting the sub-sequence.  Such cooperation is present but
;; implemented ad-hoc.  First-class support will be consistent with the
;; architecture.
;;
;; End is essentially init for going in reverse.  While using init and going
;; forward to reach the end is theoretically viable, it does extra work and
;; leads to headaches for implements.
;;
;; Goto essentially is just a careful use of step-forward.  If every forward
;; step properly reports its maximum extent of progress, we can use forward and
;; init to implement every goto.
;;
;; Finally, step-forward and step-backward should navigate the states between
;; init / end and final.
;;
;; A lazy implementer can forego methods by delegating them to simpler
;; idempotent methods, such as using an idempotent init for step-backward.  With
;; a maximum of six methods and a minimum of two, just init and forward, you
;; have enough behavior to properly fit the user interface.

;; Generics.  TODO check on the use of generics.
(cl-defgeneric ms-init (obj))

(cl-defgeneric ms-end (obj))

(cl-defgeneric ms-final (obj))

(cl-defgeneric ms-step-forward (obj))

(cl-defgeneric ms-step-backward (obj))

(cl-defgeneric ms-goto (obj point))

;; ** Stateful Sequence
(defclass ms-stateful-sequence ()
  ((parent :initval nil :initarg :parent
           "Parent or root sequence.
Usually a deck or slide."))
  "An interface definition for linear sequences of steps.
The sequence can be traversed forwards and backward and also
indexed into from higher level navigation commands.  Sequences
can run as sub-sequences, where one sequence calls into another.

Because the steps may rely on some setup and teardown, the
stateful sequence provides methods to call these functions at the
appropriate times.

Classes that wish to implement the stateful sequence interface
just need to support a few methods and then rely on the generic
implementations for the rest, unless they want to optimize or
simplify their implementation.")

(cl-defmethod ms-init ((obj ms-stateful-sequence))
  "Called when entering a sequence.
Any state that must be set up for this sequence can run during
the init method.  Init does not count as a step.  The guarantee
from callers is that if init is called, `ms-final'
will also be called.

TODO Return is currently ignored, mainly because most init
implementations are expected to produce side-effects rather than
meaningful return values.

Rather than implement this function in an idempotent way, work
together with `ms-final' to make guarantees about
initial conditions and tidyingup a sequence that has completely
run its course.

PARENT exists when the sequence is a sub-sequence.  Sub-sequences
can rely on the parent state to exist for their entire lifetime.
The parent sequence will not call its own `ms-final'
until after it calls the sub-sequence's `ms-final'."
  nil)

(cl-defmethod ms-end ((obj ms-stateful-sequence))
  "Init when going backwards.
This method should be implemented so that the state is equivalent
to having gone forward to the end of the slide.  The default
implementation calls init and then advances to the end.  This can
be inappropriate in a number of cases, and should be overridden.
Re-using init is appropriate when a proper backward
implementation is not valued.

Just as init anticipates having forward called at least once, end
should anticipate backward being called at least once.  This
allows initial narrowing and slide behavior to be signaled
properly to children and section actions."
  (ms-init obj)
  (let (extent (advanced t))
    (while advanced
      (when-let ((progress (ms-step-forward obj)))
        (setq extent progress)))
    extent))

(cl-defmethod ms-final ((obj ms-stateful-sequence))
  "Called when exiting a sequence.
Implement this method to clean up any state that would interfere
with the sequence succeeding when run again.  All side-effects
and states created by steps in the sequence or the `ms-init'
method must be cleaned up or otherwise managed or else
`ms-step-backward' and other sequences of running a presentation
will be brittle and likely fail when re-run."
  nil)

(cl-defmethod ms-step-forward ((obj ms-stateful-sequence))
  "Make on step and return the point of farthest advance.
When no progress can be made, return nil.  For steps that don't
need to advance the point, if they make progress, they should
return t or the point.  Every sequenece of `ms-step-forward'
should return nil at some point."
  nil)

(cl-defmethod ms-step-backward ((obj ms-stateful-sequence))
  "Make one step backwards and return earliest point.
Backwards steps are considered to advance to be beginning of the
extent they affect.  This enables forward and backward
implementations to act as conjugates."
  nil)

(cl-defmethod ms-goto ((obj ms-stateful-sequence) point)
  "Step forward until advancing beyond POINT.
This method can usually be implemented on top of
`ms-step-forward' by advancing until POINT is exceeded.
`ms-init' is guaranteed to have been called."
  (let (exceeded (advanced t))
    (while (and advanced (not exceeded))
      (let ((progress (ms-step-forward obj)))
        (if (and (numberp progress)
                 (>= progress point))
            (setq exceeded progress)
          (setq advanced progress))))))

;; ** Parent
;; TODO this class is kind of half-baked.  It was intended to wrap up the
;; filtering functionality and needing to find next and previous children.
;; Needs actual usage to become mature.
(defclass ms-parent ()
  ((filter :initform nil
           :initarg :filter
           :documentation "Function to filter child headings."))
  "The parent class implements methods that need to filter
children. Decks and slides have children.")

;; TODO highly indirect and delegates down to a really crappy implementation
;; that nobody else should ever want to use
(cl-defmethod ms-next-child ((obj ms-parent) child)
  "Get the next unfiltered CHILD of OBJ."
  (ms-next-sibling
   child (oref obj filter)))

(cl-defmethod ms-previous-child ((obj ms-parent) child)
  "Get the previous unfiltered CHILD of OBJ."
  (ms-previous-sibling
   child (oref obj filter)))

;; ** Deck
;; TODO extract non-org-specific behavior to sequence-root class.
(defclass ms-deck (ms-progress-tracking
                   ms-parent)
  ((slide :initform nil
          "The active sequence or slide.
This is probably a `ms-slide' object, but anything
that implements `ms-stateful-sequence' will probably
work as well.")
   (base-buffer :initform nil :initarg :base-buffer
                "Source of the slide deck.")
   (slide-buffer :initform nil :initarg :slide-buffer
                 "Indirect buffer used to display slides in.")
   (window-config :initform nil :initarg :window-config
                  "Window configuration for restoring after stop.")
   ;; TODO this implementation doesn't work if more indirect buffers are used.
   (slide-buffer-state
    :initform nil
    "Initiated by display actions to `contents' or `slides'.")
   (step-callbacks
    :initform nil
    "Steps to run before next steps.
FORM is just a list as steps will always be run before any
sequence ends or makes progress..")
   (sequence-callbacks
    :initform '(nil)
    "Steps that run only when sequences end.
Form is a list of STEPS where STEPS is a list of callbacks.  The
length of this list is equal to the depth of the current
sequence.  See `ms-push-step' for information about how to push a
step deeper into the sequences."))

  "The Deck is responsible for selecting the parent node and
maintaining state between mode activations or when switching
between slides and contents.  Class can be overridden to affect
root behaviors.  See `ms-default-deck-class'")

(cl-defmethod ms-init ((obj ms-deck))
  "For the deck class, init needs to call init on slides until one succeeds.
This could result in skipping slides that do not report any readiness during
their init."
  (unless (oref obj slide)
    ;; Calls implied from other commands should have started the lifecycle already
    (error "No slide selected"))

  (let (initialized reached-end)
    (while (and (not initialized)
                (not reached-end))
      (narrow-to-region (point) (point)) ; signal to slide to draw itself
      (let ((result (ms-init (oref obj slide))))
        ;; Might bug when init returns nil.  Most actions never return nil.
        (if result
            (setq initialized (ms-step-forward (oref obj slide)))
          (if-let ((next (ms-next-child obj (oref obj slide))))
              (oset obj slide next)
            (setq reached-end t)))))
    (when reached-end
      ;; TODO probably the resulting state just needs to act like there is no
      ;; next slide and call the `ms-after-last-slide-hook'
      (error "No slides could initialize"))))

(cl-defmethod ms-end ((obj ms-deck))
  (error "Deck has no valid concept of starting at the end."))

(cl-defmethod ms-final ((obj ms-deck))
  (when-let ((slide (oref obj slide)))
    (ms-final slide)))

;; Deck forward & backward methods implement a lot of the capability.  In the
;; function-stack analogy, the deck's forward & backward are similar to a
;; runtime, handling call and return behavior, advancing to the next slide /
;; function in our presentation / procedure etc.  Support for calling into a
;; sub-sequence or doing something upon return are baked in.  It make require
;; several trips through the behavior to consume callbacks that are run for
;; effect or are no-op, things that don't count as steps or are slides that
;; decide at runtime to be skipped.
;;
;; There are many little user-facing behaviors, such as following the slide in
;; the base buffer with the point.  These are best done from the sequence root.
;; It bloats the function, but has little effect on the complexity of the logic.
;;
;; So that sounds like a lot, but it's really simple.  Loop through whatever
;; next steps and callbacks were pushed onto the stack.  When one of them makes
;; progress, we're done.

;; TODO When slides are converted so that they push new sequences on their own,
;; we no longer need to handle the case where a child slide is returned.  I
;; think this is where the implementation is going because it's very elegant.

(cl-defmethod ms-step-forward ((obj ms-deck))
  ;; TODO Check for forward callbacks
  (unless (oref obj slide)
    ;; Calls implied from other commands should have started the lifecycle
    ;; already
    (error "No slide selected"))

  (let ((restriction-min (point-min))
        (restriction-max (point-max))
        progress reached-end)
    ;; Burn up a step callback until one returns non-nil
    ;; TODO do I need this `slot-boundp' check?
    (when-let ((steps (and (slot-boundp obj 'step-callbacks)
                           (oref obj step-callbacks))))
      (while (and (not progress)
                  steps)
        (setq progress (funcall (pop steps) 'forward)))
      (oset obj step-callbacks steps))

    (while (not (or progress reached-end))
      (let* ((current-slide (oref obj slide))
             (result (ms-step-forward current-slide))
             next-slide switching-to-parent switching-to-sibling)

        (if (eieio-object-p result)
            (setq next-slide result)
          (setq progress result))

        ;; Before we might check for a parent or next tree, check for a slide
        ;; callback and see if it can make progress.
        (unless result
          ;; Burn up a step callback until one returns non-nil
          (when-let* ((sequence-callbacks (oref obj sequence-callbacks))
                      (steps (car sequence-callbacks)))
            (while (and (not progress) steps)
              (setq progress (funcall (pop steps) 'forward)))
            ;; If all the steps for this sequence were consumed, pop a layer.
            (unless steps
              (oset obj sequence-callbacks (cdr sequence-callbacks)))))

        (unless (or progress result)
          ;; Next check if there is a parent slide, which is true unless the
          ;; parent is the deck.  Then check if there is a next sibling.
          (let* ((parent (oref current-slide parent)))
            (if (not (eq obj parent))
                (setq next-slide parent
                      switching-to-parent t)
              (if-let ((next-child (ms-next-child obj current-slide)))
                  (setq next-slide next-child
                        switching-to-sibling t)
                (setq reached-end t)))))

        (ms--debug current-slide)
        (when ms--debug
          (message "switching-to-parent: %s" switching-to-parent))
        (when next-slide
          (ms--debug next-slide))

        (when next-slide
          (unless switching-to-parent
            ;; Push a new sequence-callbacks level
            (push nil (oref obj sequence-callbacks)))

          (oset obj slide next-slide)

          ;; When switching to a parent slide, we will finalize the old slide.
          ;; When switching to a child, we will not finalize the parent.
          (cond
           (switching-to-parent
            ;; TODO slide re-entry when parent can still make progress
            (ms-final current-slide))
           (t
            (when switching-to-sibling
              (ms-final current-slide))

            ;; TODO extract behavior and add to other navigation actions
            (when ms-base-follows-slide
              (let ((pos (marker-position (oref next-slide begin))))
                (set-buffer (oref obj base-buffer))
                (unless (and (>= pos (point-min))
                             (<= pos (point-max)))
                  (widen))
                (when-let ((windows (get-buffer-window-list (current-buffer))))
                  (mapc (lambda (w) (set-window-point w pos)) windows))
                (set-buffer (oref obj slide-buffer))))

            ;; We just run the init and then let the next loop call the first
            ;; forward, handling the result of progress appropriately.
            (ms-init next-slide))))))

    ;; A lot of progress may have happened, but there will be only one feedback
    ;; message.
    (when progress
      ;; If the restriction was updated, call the after-narrow hook
      (when (not (and (= restriction-min (point-min))
                      (= restriction-max (point-max))))
        (run-hooks 'ms-narrow-hook))
      (ms--feedback :forward))

    (when reached-end
      ;; TODO exhaust any remaining next slide callbacks
      (run-hooks 'ms-after-last-slide-hook))))

(cl-defmethod ms-step-backward ((obj ms-deck))
  (unless (oref obj slide)
    ;; Calls implied from other commands should have started the lifecycle
    ;; already
    (error "No slide selected"))

  ;; Going backward is almost the same as going forward.  The big difference is
  ;; that when a slide is instantiated, it needs to be sent to its end.  Usually
  ;; the default implementation, which calls step-forward until progress is
  ;; exhausted, is fine.  Certain actions with side-effects may not like this,
  ;; and they should implement an actual `ms-end' method as well as idempotent
  ;; `ms-init' and `ms-final' if any support for going backwards is desirable.

  (let ((restriction-min (point-min))
        (restriction-max (point-max))
        progress reached-beginning)
    ;; Burn up a step callback until one returns non-nil
    (when-let ((steps (and (slot-boundp obj 'step-callbacks)
                           (oref obj step-callbacks))))
      (while (and (not progress)
                  steps)
        (setq progress (funcall (pop steps) 'backward)))
      (oset obj step-callbacks steps))

    (while (not (or progress reached-beginning))
      (let* ((current-slide (oref obj slide))
             (result (ms-step-backward current-slide))
             previous-slide switching-to-parent switching-to-sibling)

        (if (eieio-object-p result)
            (setq previous-slide result)
          (setq progress result))

        ;; Before we might check for a parent or next tree, check for a slide
        ;; callback and see if it can make progress.
        (unless result
          ;; Burn up a step callback until one returns non-nil
          (when-let* ((sequence-callbacks (oref obj sequence-callbacks))
                      (steps (car sequence-callbacks)))
            (while (and (not progress) steps)
              (setq progress (funcall (pop steps) 'backward)))
            ;; If all the steps for this sequence were consumed, pop a layer.
            (unless steps
              (oset obj sequence-callbacks (cdr sequence-callbacks)))))

        (unless (or progress result)
          ;; Next check if there is a parent slide, which is true unless the
          ;; parent is the deck.  Then check if there is a previous sibling.
          (let* ((parent (oref current-slide parent)))
            (if (not (eq obj parent))
                (setq previous-slide parent
                      switching-to-parent t)
              (if-let ((previous-child (ms-previous-child
                                        obj current-slide)))
                  (setq previous-slide previous-child
                        switching-to-sibling t)
                (setq reached-beginning t)))))

        (ms--debug current-slide)
        (when ms--debug
          (message "switching-to-parent: %s" switching-to-parent))
        (when previous-slide
          (ms--debug previous-slide))

        (when previous-slide
          (unless switching-to-parent
            ;; Push a new sequence-callbacks level
            (push nil (oref obj sequence-callbacks)))

          (oset obj slide previous-slide)

          ;; When switching to a parent slide, we will finalize the old slide.
          ;; When switching to a child, we will not finalize the parent.
          (cond
           (switching-to-parent
            ;; TODO slide re-entry when parent can still make progress?
            (ms-final current-slide))
           (t
            (when switching-to-sibling
              (ms-final current-slide))
            ;; TODO extract behavior and add to other navigation commands
            (when ms-base-follows-slide
              (let ((pos (marker-position (oref previous-slide begin))))
                (set-buffer (oref obj base-buffer))
                (unless (and (>= pos (point-min))
                             (<= pos (point-max)))
                  (widen))
                (when-let ((windows (get-buffer-window-list (current-buffer))))
                  (mapc (lambda (w) (set-window-point w pos)) windows))
                (set-buffer (oref obj slide-buffer))))
            ;; We just send the slide to its end (reverse init) and allow the
            ;; next loop to call step-backward, obtaining progress and properly
            ;; handling the result.
            (ms-end previous-slide))))))

    ;; A lot of progress may have happened, but there will be only one feedback
    ;; message.
    (cond (progress
           ;; If the restriction was updated, call the after-narrow hook
           (when (not (and (= restriction-min (point-min))
                           (= restriction-max (point-max))))
             (run-hooks 'ms-narrow-hook))
           (ms--feedback :backward))
          (reached-beginning
           (user-error "No more previous slides!")))))

(cl-defmethod ms--choose-slide ((obj ms-deck) how)
  "Set the current slide, according to HOW.
Optional POINT allows resolving a slide by walking the tree to
find the slide that displays that POINT."
  (cond ((eq how 'first)
         (oset obj slide (ms--make-slide
                          (ms--document-first-heading) obj)))
        ((eq how 'point)
         (let ((base-point (with-current-buffer (oref obj base-buffer)
                             (point))))
           ;; TODO implement looking inside the slides using `goto' and recover
           ;; the child with POINT
           (oset obj slide
                 (ms--make-slide
                  (ms--root-heading-at-point base-point) obj))))))

;; TODO probably don't need methods for the functions below
(cl-defmethod ms-deck-live-p ((obj ms-deck))
  "Check if all the buffers are alive or can be recovered."
  ;; TODO in some circumstances, an indirect buffer might exist, but we should
  ;; probably kill it if it was created outside the current instance's lifecycle
  (and (buffer-live-p (oref obj base-buffer))
       (buffer-live-p (oref obj slide-buffer))
       (eq (oref obj base-buffer) (buffer-base-buffer
                                   (oref obj slide-buffer)))))

(defun ms-push-kill-buffer ()
  "Push a step to kill the current buffer.")

(defun ms-push-window-config (&optional pop-when step)
  "Save the window configuration and narrowing for restoration.
POP-WHEN will add a callback to restore the restriction.

Optional POP-WHEN decides when to restore the config.  See
`ms-push-step' for details.

Optional STEP argument will decide if the callback counts as a step or will
return nil so that it is only run for effects."
  (let ((window-config (current-window-configuration)))
    (ms-push-step
     (lambda (_) (prog1 step
              (set-window-configuration window-config)))
     pop-when)))

(defun ms-push-restriction (&optional pop-when step)
  "Save the current buffer restriction for restoration.
POP-WHEN will add a callback to restore the restriction.

Optional POP-WHEN decides when to restore the config.  See
`ms-push-step' for details.

Optional STEP argument will decide if the callback counts as a step or will
return nil so that it is only run for effects."
  (let* ((begin (point-min-marker))
         (end (point-max-marker))
         (size (buffer-size))
         (restricted (/= (- end begin) size)))
    (ms-push-step
     (lambda (_) (prog1 step
              (if restricted
                  (narrow-to-region begin end)
                (widen))))
     pop-when)))

;; TODO pop the sequence root on stop.
(defun ms-push-step (fun &optional pop-when)
  "Run FUN as next step.
FUN is a function of a single argument, `forward' or `backward'.

The return value is interpreted as progress, so return non-nil if
you want FUN to count as a step or nil if FUN is only run for
effects.

Optional POP-WHEN argument means run FUN after current sequence
ends.  Three kinds of values are understood:

- `step' or nil: next step.

- integer: Depth of parents.  0 is the current sequence.

- `sequence': always equivalent to 0.  Just run when the current
  sequence ends.  TODO ⚠️ This could be unstable if actions become
  sub-sequences, which is currently intended.

- `root': run before cleanup.  Equivalent to passing an integer
  equal to one less than the sequence depth.

If you need multiple steps, consider adding steps inside of FUN
for recursive dynamic steps rather than adding a lot of steps at
once, which requires the functions to be removed or return nil."
  (unless (ms-live-p)
    (error "No active deck"))
  (cond ((eq pop-when 'step)
         (push fun (oref ms--deck step-callbacks)))
        ((or (eq pop-when 'sequence)
             (eq pop-when 0))
         (push fun (car (oref ms--deck sequence-callbacks))))
        ((integerp pop-when)
         (if (>= pop-when (length (oref ms--deck sequence-callbacks)))
             (error "Requested depth exceeds sequence depth")
           (push fun (nth pop-when
                          (oref ms--deck sequence-callbacks)))))))

;; * Slide
(defclass ms-slide (ms-parent ms-stateful-sequence)
  ((slide-action :initform nil :initarg :slide-action
                 :description "Action run after section.
See `ms-default-child-action'.")
   (section-actions :initform nil :initarg :section-actions
                    :description "Actions run within the section display
lifecycle.  See `ms-default-section-actions'.")
   (child-action :initform nil :initarg :child-action
                 :description "Action run after section.
See `ms-default-child-action'.")
   (begin :initform nil :initarg :begin
          :description "Marker for retrieving this heading's org element."))
  "Slides store some local state and delegate behavior to several
functions. The Slide is a stateful node that hydrates around a
heading and stores actions and their states.")

(cl-defmethod ms-init ((obj ms-slide))
  (when-let ((display-action (oref obj slide-action)))
    (ms-init display-action))
  (mapc (lambda (action)
          (ms-init action))
        (oref obj section-actions))
  (when-let ((child-action (oref obj child-action)))
    (ms-init child-action))
  ;; TODO this t is just a hack.  The implementation of reacting to return
  ;; values from init has been in flux.
  t)

(cl-defmethod ms-end ((obj ms-slide))
  (when-let ((child-action (oref obj child-action)))
    (ms-end child-action))
  (mapc (lambda (action)
          (ms-end action))
        (reverse (oref obj section-actions)))
  (when-let ((display-action (oref obj slide-action)))
    (ms-end display-action)))

(cl-defmethod ms-final ((obj ms-slide))
  (when-let ((display-action (oref obj slide-action)))
    (ms-final display-action))
  (mapc (lambda (action)
          (ms-final action))
        (oref obj section-actions))
  (when-let ((child-action (oref obj child-action)))
    (ms-final child-action))
  ;; Clean up marker
  (set-marker (oref obj begin) nil))

(cl-defmethod ms-step-forward ((obj ms-slide))
  (let ((section-actions (oref obj section-actions))
        progress)
    (setq progress (when-let ((display-action (oref obj slide-action)))
                     (ms-step-forward display-action)))
    (while (and (not progress)
                section-actions)
      (let ((action (pop section-actions)))
        (when-let ((result (ms-step-forward action)))
          (setq progress result))))
    (or progress
        (when-let ((child-action (oref obj child-action)))
          (ms-step-forward child-action)))))

(cl-defmethod ms-step-backward ((obj ms-slide))
  (let ((section-actions (reverse (oref obj section-actions)))
        progress)
    ;; section display action happens before any section-actions
    (setq progress (or (when-let ((child-action (oref obj child-action)))
                         (ms-step-backward child-action))
                       (when-let ((display-action
                                   (oref obj slide-action)))
                         (ms-step-backward display-action))))
    (while (and (not progress)
                section-actions)
      (let ((action (pop section-actions)))
        (when-let ((result (ms-step-backward action)))
          (setq progress result))))
    progress))

;; `ms--make-slide' is very critical to the user-facing configuration and
;; hacker-facing capabilities and API.  Slides are hydrated from org mode
;; headings.  We can pretty much divide the likely user needs into either what
;; to do with the section and what to do with the child headings.  Because the
;; section needs to be narrowed to, and this narrowing must be performed both
;; forwards and backwards, we also have a slide action that is run around the
;; section and child actions.
;;
;; It was anticipated for a time that actions might be nested in the
;; configuration.  However, we still have a likely need for configuring just the
;; section action or just the child action, and this API is not expected to look
;; that different to the user whether nesting of actions is supported or not.
;;
;; Both child actions and user configuration have demonstrated a large benefit
;; from being able to slightly change the behavior of actions.  This is why the
;; plist arguments are supported when hydrating from org properties and child
;; actions can pass in arguments to `ms--make-slide'.

(defun ms--make-slide (heading parent &rest args)
  "Hydrate a slide object from a HEADING element.
Many optional ARGS.  See code."
  ;; function doesn't error but results in nil begin marker if we don't fail
  (unless heading
    (error "No heading provided"))
  (unless parent
    (error "No parent provided"))

  ;; Share the beginning marker across all actions.  It's not unique and
  ;; shouldn't move.
  ;; TODO Consolidate explicit nil indication around whatever is standard
  (let* ((begin-position (org-element-begin heading))
         (begin (make-marker))
         (slide-action-class (plist-get args :slide-action))
         (slide-action-args (plist-get args :slide-action-args))
         ;; TODO Haven't needed to specify section actions from the parent yet
         ;; actions.
         ;; Child action class can be `none' for explicit nil
         (child-action-class (plist-get args :child-action))
         (child-action-args (plist-get args :child-action-args)))

    (set-marker begin begin-position (current-buffer))

    ;; Hydrate the slide's configuration as classes and arguments and then
    ;; instantiate them all.
    (let* ((keywords (org-collect-keywords
                      '("SLIDE_ACTION"
                        "SLIDE_SECTION_ACTIONS"
                        "SLIDE_CHILD_ACTION"
                        "SLIDE_FILTER"
                        "SLIDE_CLASS")))

           ;; TODO just munged this a bit for explicit nil handling.  Might
           ;; still have precedence wrong.  If there is any string set in any
           ;; property, the default value shouldn't be used.
           (slide-action-class
            (or slide-action-class
                (if-let ((declared
                          (or (org-element-property :SLIDE_ACTION heading)
                              (cdr (assoc-string "SLIDE_ACTION"
                                                 keywords)))))
                    (ms--parse-class-with-args declared)
                  ms-default-slide-action)))

           ;; TODO precedences are out of wack.  Heading property should win
           ;; versus child heading, document, or default
           (slide-action (when slide-action-class
                           (if (consp slide-action-class)
                               (apply (car slide-action-class)
                                      :begin begin
                                      (append slide-action-args
                                              (cdr slide-action-class)))
                             (apply slide-action-class
                                    :begin begin
                                    slide-action-args))))

           ;; TODO action arguments might make sense, such as telling nested
           ;; elements not to animate.  It's really hard for them to infer being
           ;; in an inline child versus an independent slide, even by looking at
           ;; the restriction.
           (section-action-classes
            (or (ms--parse-classes-with-args
                 (or (org-element-property :SLIDE_SECTION_ACTIONS heading)
                     (cdr (assoc-string "SLIDE_SECTION_ACTIONS" keywords))))
                ms-default-section-actions))
           (section-actions
            (mapcar
             (lambda (c) (when c
                      (if (consp c)
                          (apply (car c) :begin begin (cdr c))
                        (funcall c :begin begin))))
             section-action-classes))

           ;; TODO Likely some precedence funk here.  Copied from above.
           (child-action-class
            (or child-action-class
                (if-let ((declared
                          (or (org-element-property :SLIDE_CHILD_ACTION heading)
                              (cdr (assoc-string "SLIDE_CHILD_ACTION"
                                                 keywords)))))
                    (ms--parse-class-with-args declared)
                  ms-default-child-action)))

           (child-action (when (and child-action-class
                                    (not (eq child-action-class 'none)))
                           (if (consp child-action-class)
                               (apply (car child-action-class)
                                      :begin begin
                                      (append child-action-args
                                              (cdr child-action-class)))
                             (apply child-action-class
                                    :begin begin
                                    child-action-args))))

           (filter
            (or (ms--filter
                 (or (org-element-property :SLIDE_FILTER heading)
                     (cdr (assoc-string "SLIDE_FILTER" keywords))))
                ms-default-filter))
           (class
            (or (ms--parse-class-with-args
                 (or (org-element-property :SLIDE_CLASS heading)
                     (cdr (assoc-string "SLIDE_CLASS"
                                        keywords))))
                ms-default-class)))

      (let ((slide (apply (if (consp class) (car class) class)
                          :slide-action slide-action
                          :section-actions section-actions
                          :child-action child-action
                          :filter filter
                          :parent parent
                          :begin begin
                          (when (consp class)
                            (cdr class)))))
        slide))))

(cl-defmethod ms-next-sibling ((obj ms-slide) filter)
  (when-let* ((heading (ms-heading obj))
              (next-heading (ms--next-sibling heading filter)))
    (ms--make-slide next-heading (oref obj parent))))

(cl-defmethod ms-previous-sibling ((obj ms-slide) filter)
  (when-let* ((heading (ms-heading obj))
              (previous-heading (ms--previous-sibling heading filter)))
    (ms--make-slide previous-heading (oref obj parent))))

(cl-defmethod ms-heading ((obj ms-slide))
  "Return the slide's heading element."
  (org-element-at-point (oref obj begin)))

;; * Actions
;;; Pre-built Actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Actions are stateful sequences.  They live on a slide.  They usually work on
;; either the section or the children, but there is no requirement that they are
;; exclusive to either.  Child actions should compose with section actions, such
;; as round-robin children cycling through each child's action's forward and
;; backward methods. TODO TODO TODO 🚧

;; ** Base Action
(defclass ms-action (ms-stateful-sequence
                     ms-progress-tracking)
  ((begin :initform nil :initarg :begin "Marker for beginning of heading.
Used to re-hydrate the org element for use in mapping over the section etc."))
  "Base class for most slide actions that work on a heading's contents."
  :abstract t)

(cl-defmethod ms-heading ((obj ms-action))
  "Return the slide's heading element."
  (let ((heading (org-element-at-point (oref obj begin))))
    (if (eq (org-element-type heading) 'headline)
        heading
      (error "Begin marker no longer points at a heading"))))

(cl-defmethod ms-section-next
  ((obj ms-action) type &optional pred info no-recursion)
  "Move marker forward by one org element of TYPE and return element.
Marker is moved to the end of the heading if no matching element
is found."
  (if-let ((next (ms--section-next (ms-heading obj)
                                   type
                                   (ms-marker obj)
                                   pred info no-recursion)))
      (prog1 next
        (ms-marker obj (org-element-begin next)))
    (ms-marker obj (org-element-end (ms-heading obj)))
    nil))

(cl-defmethod ms-section-previous
  ((obj ms-action) type &optional pred info no-recursion)
  "Move marker backward by one org element of TYPE and return element.
Marker is moved to the beginning of the heading if no matching
element is found."
  (if-let ((previous (ms--section-previous (ms-heading obj)
                                           type
                                           (ms-marker obj)
                                           pred info no-recursion)))
      (prog1 previous
        (ms-marker obj (org-element-begin previous)))
    (ms-marker obj (org-element-begin (ms-heading obj)))
    nil))

(cl-defmethod ms-section-map
  ((obj ms-action) type fun &optional info first-match no-recursion)
  "Map FUN over TYPE elements in SLIDE section.
FIRST-MATCH only finds the first non-nil returned from FUN.
NO-RECURSION will avoid descending into children."
  (ms--section-map
   (ms-heading obj)
   type fun info first-match no-recursion))

(cl-defmethod ms-init ((obj ms-action))
  (ms-marker obj (org-element-begin (ms-heading obj))))

(cl-defmethod ms-end ((obj ms-action))
  (ms-marker obj (org-element-end (ms-heading obj))))

(cl-defmethod ms-final ((obj ms-action))
  (when-let ((marker (oref obj marker)))
    (set-marker marker nil)))

;; TODO make a child base class.  Section actions don't really need this.
(cl-defmethod ms-forward-child ((obj ms-action))
  "Return the next direct child heading and advance the marker.
Marker is moved to the end of the heading if no matching child is
found."
  (if-let* ((marker (ms-marker obj))
            (heading (ms-heading obj))
            (target-level (1+ (org-element-property :level heading)))
            (next (ms--contents-map
                   heading 'headline
                   (lambda (child)
                     (and (= target-level (org-element-property :level child))
                          (> (org-element-begin child) marker)
                          child))
                   nil t)))
      (prog1 next
        (ms-marker obj (org-element-begin next)))
    (ms-marker obj (org-element-end (ms-heading obj)))
    nil))


(cl-defmethod ms-backward-child ((obj ms-action))
  "Return previous direct child heading and advance the marker backward.
Marker is moved to the beginning of the heading if no matching
child is found."
  (if-let* ((marker (ms-marker obj))
            (heading (ms-heading obj))
            (target-level (1+ (org-element-property :level heading)))
            ;; We have to get all the children and find the last match
            (next (car
                   (last
                    (ms--contents-map
                     heading 'headline
                     (lambda (child)
                       (and (= target-level (org-element-property :level child))
                            (< (org-element-begin child) marker)
                            child)))))))
      (prog1 next
        (ms-marker obj (org-element-begin next)))
    (ms-marker obj (org-element-begin (ms-heading obj)))
    nil))

;; ** Default Slide Action
(defclass ms-action-narrow (ms-action)
  ((include-restriction :initform nil :initarg :include-restriction
                        "Include the existing restriction.")
   (with-children :initform nil :initarg :with-children
                  "Narrow should include children.
The default, nil, narrows to the section only.")
   (last-progress :initform nil
                  "A helpful hack to prevent unintended repeat
narrowing in the lifecycle.  This is a latch variable."))
  "Default slide action.
Most actions need the current slide to be narrowed to.  This
action is capable of performing such narrowing and informing the
deck of progress was made.")

(cl-defmethod ms-narrow ((obj ms-action-narrow))
  "Narrow to this slide's heading."
  (let* ((progress)
         (heading (ms-heading obj))
         (begin (oref obj begin))
         (end (if (oref obj with-children)
                  (org-element-end heading)
                (ms--section-end heading))))

    (if (oref obj include-restriction)
        (unless (and (<= (point-min) begin)
                     (>= (point-max) end))
          (narrow-to-region (min (point-min) begin)
                            (max (point-max) end))
          (when ms-slide-in-effect
            (ms-animation-setup begin end))
          (setq progress t))
      (unless (and (<= (point-min) begin)
                   (>= (point-max) end))
        (narrow-to-region begin end)
        (ms--make-header)
        (goto-char (point-min))         ; necessary to reset the scroll
        (when ms-slide-in-effect
          (ms-animation-setup begin end))
        (setq progress t)))
    ;; This progress is important because it's how we show a slide and count as
    ;; a first step
    progress))

(cl-defmethod ms-step-forward ((obj ms-action-narrow))
  (prog1 (unless (eq 'forward (oref obj last-progress))
           (ms-narrow obj))
    (oset obj last-progress 'forward)))

(cl-defmethod ms-step-backward ((obj ms-action-narrow))
  (prog1 (unless (eq 'backward (oref obj last-progress))
           (ms-narrow obj))
    (oset obj last-progress 'backward)))

;; ** Reveal items section action
(defclass ms-action-item-reveal (ms-action)
  ((overlays :initform nil))            ; TODO see if this fixes the slot
                                        ; unbound issues
  "Hide all items and then reveal them one by one.")

;; TODO may try to read uninitialized slot...
(cl-defmethod ms-init :after ((obj ms-action-item-reveal))
  (oset obj overlays (ms-section-map obj 'item #'ms-hide-element)))

;; The default `ms-end' method is sufficient since this action will
;; just add overlays starting from the end of items.

(cl-defmethod ms-final :after ((obj ms-action-item-reveal))
  (when-let ((overlays (oref obj overlays)))
    (mapc #'delete-overlay overlays)))

;; TODO add hide / un-hide methods to the base action
(cl-defmethod ms-step-forward ((obj ms-action-item-reveal))
  ;; The implementation has mapped all of the items into overlays, so instead of
  ;; calling `ms-section-next', we just use the overlay positions to walk
  ;; through the items.
  (when-let* ((overlays (oref obj overlays))
              (first (car overlays))
              (end (overlay-end first)))
    ;; TODO We can let-bind animations false for child slides.
    ;; Or handle this via arguments in child actions
    (when ms-slide-in-effect
      (ms-animation-setup
       (overlay-start first) (overlay-end first)))
    (delete-overlay first)
    (oset obj overlays (cdr overlays))
    (ms-marker obj end)
    t))

(cl-defmethod ms-step-backward ((obj ms-action-item-reveal))
  (when-let ((previous-item (ms-section-previous obj 'item)))
    (oset obj overlays
          (cons (ms-hide-element previous-item)
                (and (slot-boundp obj 'overlays)
                     (oref obj overlays))))
    t))

;; ** Babel Action

;; TODO automatically map the blocks during init and remove results... this is
;; kind of implemented but seems to inconsistently work.
;; TODO configure results removal behavior with an argument
;; TODO any display jank concerns due to results?  Possibly inhibit re-display.
(defclass ms-action-babel (ms-action)
  () "Execute source blocks as steps.
By default blocks execute one by one with step-forward.  You can mark a block to
be special with the keyword:

- #+attr_method: init

- #+attr_method: step-forward

- #+attr_method: step-backward

- #+attr_method: end

- #+attr_method: final

These keywords correspond to the normal methods of the stateful
sequence class.  For blocks that should not occur more than once,
only the first block found will actually be executed.")

(cl-defmethod ms--clear-results ((obj ms-action-babel))
  (without-restriction
    (ms-section-map
     obj 'src-block
     (lambda (e)
       (save-excursion
         (goto-char (org-element-begin e))
         (org-babel-remove-result-one-or-many nil))))))

(defun ms--method-block-pred
    (method-name &optional unnamed)
  "Return a predicate to match the METHOD-NAME.
Optional UNNAMED will return unnamed blocks as well."
  (lambda (block)
    (if-let ((names (org-element-property :attr_method block)))
        (when (member method-name names)
          block)
      (when unnamed
        block))))

(defun ms--block-execute (block-element)
  (without-restriction
    (save-excursion
      (goto-char (org-element-begin block-element))
      ;; t for don't cache.  We likely want effects
      (org-babel-execute-src-block t))))

(cl-defmethod ms--get-block
  ((obj ms-action) &optional method-name)
  "Execute the block with keyword value METHOD-NAME.
The keywords look like:

#+attr_method: METHOD-NAME

The possible values for METHOD-NAME correspond to the
stateful-sequence class methods.  METHOD-NAME is a string."
  (let ((predicate (ms--method-block-pred method-name)))
    (ms-section-map obj 'src-block predicate nil t)))

(cl-defmethod ms-step-forward ((obj ms-action-babel))
  (when-let* ((predicate (ms--method-block-pred
                          "step-forward" t))
              (next (ms-section-next obj 'src-block predicate)))
    (or (ms--block-execute next)
        ;; If we found a next block, we made progress regardless of the block's
        ;; return value
        t)))

(cl-defmethod ms-step-backward ((obj ms-action-babel))
  (when-let* ((predicate (ms--method-block-pred
                          "step-backward"))
              (prev (ms-section-previous obj 'src-block predicate)))
    (or (ms--block-execute prev)
        ;; If we found a previous block, we made progress regardless of the
        ;; block's return value
        t)))

(cl-defmethod ms-init :after ((obj ms-action-babel))
  (when-let ((block-element (ms--get-block obj "init")))
    (ms--clear-results obj)
    (ms--block-execute block-element))
  ;; TODO pesky return values for init methods
  ;; These should probably need to be some explicit symbol to do anything other
  ;; than proceed in a care-free manner.
  t)

(cl-defmethod ms-end :after ((obj ms-action-babel))
  (when-let ((block-element (ms--get-block obj "end")))
    (ms--clear-results obj)
    (ms--block-execute block-element)))

(cl-defmethod ms-final :after ((obj ms-action-babel))
  (when-let ((block-element (ms--get-block obj "final")))
    (ms--block-execute block-element)
    (ms--clear-results obj)))

;; ** Image Action

(defclass ms-action-image (ms-action)
  ((kill-buffer :initform nil :initarg :kill-buffer)
   (include-linked :initform t :initarg :include-linked)
   (refresh :initform t :initarg :refresh))
  "Show images fullscreen in a buffer.")

(cl-defmethod ms-init :after ((obj ms-action-image))
  (org-display-inline-images
   (oref obj include-linked)
   (oref obj refresh)
   (org-element-begin (ms-heading obj))
   (org-element-end (ms-heading obj))))

;; TODO implementation relies on org link opening.  Does not check for file or
;; check that image mode displays the link correctly.
;; TODO make it just a link action?
(cl-defmethod ms-step-forward ((obj ms-action-image))
  (when-let ((link (ms-section-next obj 'link)))
    (ms-push-window-config 'step nil)

    ;; TODO success detection
    (let ((org-link-frame-setup '((file . find-file)))
          (display-buffer-overriding-action '(display-buffer-full-frame)))
      (org-link-open link))

    (when (eq (buffer-local-value 'major-mode (current-buffer))
              'image-mode)
      (image-transform-fit-to-window)
      (let ((image-buffer (current-buffer)))
        (ms-push-step
         (lambda (_)
           (when (buffer-live-p image-buffer)
             (if (oref obj kill-buffer)
                 (kill-buffer image-buffer)
               (bury-buffer image-buffer)))))))
    ;; If we found a next image, progress was made
    t))

(cl-defmethod ms-step-backward ((obj ms-action-image))
  (when-let ((link (ms-section-previous obj 'link)))
    (ms-push-window-config 'step nil)

    ;; TODO success detection
    (let ((org-link-frame-setup '((file . find-file)))
          (display-buffer-overriding-action '(display-buffer-full-frame)))
      (org-link-open link))

    (when (eq (buffer-local-value 'major-mode (current-buffer))
              'image-mode)
      (image-transform-fit-to-window)
      (let ((image-buffer (current-buffer)))
        (ms-push-step
         (lambda (_)
           (when (buffer-live-p image-buffer)
             (if (oref obj kill-buffer)
                 (kill-buffer image-buffer)
               (bury-buffer image-buffer)))))))
    ;; If we found a next image, progress was made
    t))

;; ** Default Child Action
(defclass ms-child-action-slide (ms-action) ()
  "Default child action.  Children are independent slides.")

(cl-defmethod ms-step-forward ((obj ms-child-action-slide))
  ;; For child slides, we make a slide out of the next child heading and advance
  ;; our progress forward to the end of that child
  (when-let ((child (ms-forward-child obj)))
    ;; TODO convert this to a push-sequence call
    (ms--make-slide child (oref ms--deck slide))))

(cl-defmethod ms-step-backward ((obj ms-child-action-slide))
  ;; For child slides, we make a slide out of the previous child heading and
  ;; advance our progress backward to the beginning of that child
  (when-let ((child (ms-backward-child obj)))
    ;; TODO convert this to a push-sequence call
    (ms--make-slide child (oref ms--deck slide))))

;; ** Inline Child Action
;; While the basics of making a child out of the next heading are the same, an
;; action that controls children on its own does not return them to the deck.
;; It needs to update the buffer restriction as necessary, call lifecycle
;; functions, and pass through calls to step forward.

;; TODO round-robin child action
;; TODO every-child action

;; TODO override the child's own child action
(defclass ms-child-action-inline (ms-action)
  ((children :initform nil "Children that have been instantiated."))
  "Display children inline with the parent.")

(cl-defmethod ms-step-forward ((obj ms-child-action-inline))
  (let (progress exhausted)
    (while (not (or progress exhausted))
      ;; First try the most recently added child
      (setq progress (when-let* ((child (car (oref obj children))))
                       (ms-step-forward child)))

      ;; If the child didn't make progress, try to load up the next child
      (unless progress
        (if-let* ((child-heading (ms-forward-child obj))
                  (child (ms--make-slide
                          child-heading
                          (oref ms--deck slide)
                          :slide-action #'ms-action-narrow
                          :slide-action-args '(:include-restriction t :with-children t)
                          :child-action 'none))
                  (success (ms-init child)))
            (push child (oref obj children))
          (setq exhausted t))))
    ;; Don't return any child objects to the deck or it will treat them like
    ;; slides
    (not (null progress))))

(cl-defmethod ms-step-backward ((obj ms-child-action-inline))
  (let (progress)
    (while (and (oref obj children) (not progress))
      ;; First try the most recently added child
      (setq progress (when-let* ((child (car (oref obj children))))
                       (ms-step-backward child)))

      ;; If the child didn't make progress, narrow it away
      (unless progress
        (let ((finished (pop (oref obj children)))
              (heading (ms-backward-child obj))) ; for marker effects 💡
          (when heading
            ;; TODO narrow's final method can handle this 😼
            (narrow-to-region (point-min)
                              (org-element-begin heading)))
          (ms-final finished)
          (setq progress t))))
    ;; Don't return any child objects to the deck or it will treat them like
    ;; slides
    (not (null progress))))

(cl-defmethod ms-end :after ((obj ms-child-action-inline))
  ;;  Basically the default stateful sequence technique
  (ms-marker obj (org-element-begin (ms-heading obj)))
  (while (ms-step-forward obj) t))

(cl-defmethod ms-final :after ((obj ms-child-action-inline))
  (mapc #'ms-final (oref obj children)))

;; * Filters

(defun ms-built-in-filter (heading)
  "HEADING is an org element.
Return the heading unless it's filtered."
  ;; TODO implement.  This is not particularly hard.  The filtering must be done
  ;; according to the parent's predicate.  Slides and decks implement parent.
  ;; Actions should use their parent's predicate.
  heading)

;; * Hiding Elements

;; Use of the hide-* functions assumes the tree is within the buffer narrowing
;; restriction and also not folded.  Try `org-fold-show-subtree' and
;; `org-cycle-tree' before calling if strange behavior is observed.

;; TODO keep-lines seems kind of slow
;; TODO move this up into the action class and pass through to these as private
;; methods.
(defun ms-hide-region (beg end &optional keep-lines)
  "Return overlay hiding region between BEG and END.
Optional KEEP-LINES will replace region with as many newlines as
the region contains, preserving vertical size."
  (let ((ov (make-overlay beg end))
        (lines (if keep-lines
                   (let ((found 0))
                     (save-excursion
                       (goto-char beg)
                       (while (re-search-forward "\n" end t)
                         (setq found (1+ found))))
                     found)
                 0)))
    (overlay-put ov 'display (make-string lines ?\n))
    ov))

(defun ms-hide-element (element &optional keep-lines)
  "Return an overlay that will hide ELEMENT.
Element is an org element. Optional KEEP-LINES will replace
region with as many newlines as the region contains, preserving
vertical size."
  (ms-hide-region (org-element-begin element)
                  (org-element-end element)
                  keep-lines))

(defun ms-hide-item (item &optional keep-lines)
  "Return an overlay that hides ITEM.
See `org-item-struct' for structure of ITEM.  Note, this hides
the entire item, which may contain sub-items, but revealing
children of a hidden parent doesn't really make sense.

Optional KEEP-LINES will replace region with as many newlines as
the region contains, preserving vertical size."
  (ms-hide-region
   (car item) (car (last item)) keep-lines))

(defun ms-hide-contents (element &optional keep-lines)
  "Return an overlay that hides the contents of ELEMENT.
Element is an org element.  You should probably not use this on
headings because their contents includes the sections and the
children.

Optional KEEP-LINES will replace region with as many newlines as
the region contains, preserving vertical size."
  (ms-hide-region (org-element-contents-begin element)
                  (org-element-end element)
                  keep-lines))

(defun ms-hide-section (heading &optional keep-lines)
  "Return an overlay that hides the section of ELEMENT.
Element is an org element.  You should probably not use this on
headings because their section includes the sections and the
children.

Optional KEEP-LINES will replace region with as many newlines as
the region contains, preserving vertical size."
  (ms-hide-region
   (ms--section-begin heading)
   (ms--section-end heading)
   keep-lines))

;; * Element Mapping

;; Functions of headings are private so that corresponding slide methods can be
;; public.  Private methods with public counterparts are at least as stable as
;; the public method.

(defun ms--map
    (element type fun &optional info first-match no-recursion)
  "Map over the contents of the ELEMENT.
TYPE and FUNCTION are described in `org-element-map'."
  (let ((type (if (listp type) type (list type))))
    (save-excursion
      (save-restriction
        (narrow-to-region (org-element-begin element)
                          (org-element-end element))
        (let ((data (org-element-parse-buffer)))
          (org-element-map data type fun info
                           first-match no-recursion))))))

(defun ms--contents-map
    (element type fun &optional info first-match no-recursion)
  "Map over the contents of the ELEMENT.
TYPE and FUNCTION are described in `org-element-map'."
  (let ((type (if (listp type) type (list type))))
    (save-excursion
      (save-restriction
        (when-let ((begin (org-element-contents-begin element))
                   (end (org-element-contents-end element)))
          (narrow-to-region begin end)
          (let ((data (org-element-parse-buffer)))
            (org-element-map data type fun info
                             first-match no-recursion)))))))

(defun ms--section-map
    (heading type fun &optional info first-match no-recursion)
  "Map the SECTION of HEADING.
This includes all text up to the first child."
  (when-let ((section (ms--section heading)))
    (ms--map section type fun info
             first-match no-recursion)))

(defun ms--section-next
    (heading type after &optional pred info no-recursion)
  "Return next element of TYPE that begins after AFTER.
Optional PRED should accept ELEMENT and return non-nil if
matched."
  (let* ((combined-pred (ms-and
                         pred
                         (lambda (e) (> (org-element-begin e) after)))))
    (ms--section-map
     heading type combined-pred info t no-recursion)))

(defun ms--section-previous
    (heading type before &optional pred info no-recursion)
  "Return previous element of TYPE that starts before BEFORE.
Optional PRED should accept ELEMENT and return non-nil if
matched."
  (let* ((combined-pred (ms-and
                         pred
                         (lambda (e) (< (org-element-begin e) before)))))
    ;; We can't map in reverse, so just retrieve all matched elements and
    ;; return the last one.
    (car (last (ms--section-map
                heading type combined-pred info nil no-recursion)))))

(defun ms--section (heading)
  "Get the section of a HEADING."
  (ms--map
   heading 'section #'identity nil t t))

(defun ms--section-begin (heading)
  "Always returns a point, even for empty headings."
  (if-let ((section (ms--map
                     heading 'section #'identity nil t t)))
      (org-element-begin section)
    (or (org-element-contents-begin heading)
        (org-element-end heading))))

(defun ms--section-end (heading)
  "Always returns a point, even for empty headings."
  (let ((not-self (lambda (e) (unless (equal (org-element-begin e)
                                        (org-element-begin heading))
                           e))))
    (if-let ((section-or-heading (ms--map
                                  heading '(headline section)
                                  not-self nil t t)))
        (if (eq (org-element-type section-or-heading)
                'headline)
            (org-element-begin section-or-heading)
          (org-element-end section-or-heading))
      (or
       (org-element-contents-begin heading)
       (org-element-end heading)))))

;; TODO these two functions behaved badly and rely on non-element methods of
;; unknown behavior
(defun ms--previous-sibling (heading &optional predicate)
  "Return the previous sibling HEADING if it exists.
PREDICATE should accept an ELEMENT argument and return non-nil."
  (without-restriction
    (save-excursion
      (goto-char (org-element-begin heading))
      (let* ((predicate (or predicate #'identity))
             found)
        (while (and (> (point) (point-min))
                    (not found)
                    (org-get-previous-sibling))
          (let ((element (org-element-at-point)))
            (when (and (eq (org-element-type element) 'headline)
                       (funcall predicate element))
              (setq found element))))
        found))))

(defun ms--next-sibling (heading &optional predicate)
  "Return the next sibling HEADING if it exists.
PREDICATE should accept an ELEMENT argument and return non-nil."
  (without-restriction
    (save-excursion
      (goto-char (org-element-begin heading))
      (let* ((predicate (or predicate #'identity))
             found)
        (while (and (< (point) (point-max))
                    (not found)
                    (org-get-next-sibling))
          (let ((element (org-element-at-point)))
            (when (and (eq (org-element-type element) 'headline)
                       (funcall predicate element))
              (setq found element))))
        found))))

(defun ms--list-item-contains (item loc)
  (when item
    (let ((beg (car item))
          (end (car (last item))))
      (and  (>= loc beg)
            (< loc end)))))

(defun ms-type-p (element-or-type type)
  "Check element TYPE.
ELEMENT-OR-TYPE can be a type symbol or an org element.  TYPE can
be a list of types or a type from `org-element-all-elements.'"
  (when-let ((element-type (or (when (symbolp element-or-type)
                                 element-or-type)
                               (and element-or-type
                                    (org-element-type
                                     element-or-type)))))
    (if (listp type)
        (member element-type type)
      (eq element-type type))))

(defun ms--child-predicate (heading &optional predicate)
  "Returns a predicate to filter direct children matching PREDICATE.
PREDICATE should return matching children."
  (let ((level (org-element-property :level heading))
        (predicate (or predicate #'identity)))
    (lambda (child)
      (and (= (1+ level) (org-element-property :level child))
           (funcall predicate child)
           child))))

(defun ms--heading-p (element)
  "Really wish they would just normalize headline and heading."
  (ms-type-p element 'headline))

(defun ms--element-root (element &optional type)
  "Get the root parent of ELEMENT of TYPE.
TYPE is a list or type symbol."
  (let ((parent (org-element-parent element)))
    (while parent
      (if (or (not type)
              (ms-type-p parent type))
          (setq element parent
                parent (org-element-parent parent))
        (setq parent nil)))
    element))

(defun ms--document-first-heading ()
  "Return the first heading element"
  (save-restriction
    (widen)
    (save-excursion
      (let ((buffer-invisibility-spec nil))
        (goto-char (point-min))
        (let ((first-element (org-element-at-point)))
          (if (and first-element
                   (eq (org-element-type first-element)
                       'headline))
              (org-element-at-point)
            (when (re-search-forward org-outline-regexp-bol nil t)
              (goto-char (match-beginning 0))
              (org-element-at-point))))))))

(defun ms--root-heading-at-point (&optional point)
  "Return the root heading if the point is contained by one.
Does not modify the point."
  (save-excursion
    (when point
      (goto-char point))
    (let* ((element (org-element-at-point))
           (parent (ms--element-root
                    element 'headline)))
      (if (eq 'headline (org-element-type element))
          element
        (or parent
            (ms--any-heading))))))

(defun ms--any-heading ()
  "Return any heading that can be found.
Does not modifiy the point."
  (save-excursion
    (if (not (numberp (org-back-to-heading-or-point-min)))
        (org-element-at-point)
      (when (re-search-forward org-heading-regexp)
        (org-back-to-heading)
        (org-element-at-point)))))

(defun ms-and (&rest predicates)
  "Combine PREDICATES for filtering elements.
Each predicate should take one argument, an org element."
  (lambda (element)
    (seq-reduce
     (lambda (init pred)
       (when (or (not pred)
                 (and init (funcall pred init)))
         init))
     predicates element)))

;; * Slide Header

;; These variables were brought forward from `ms'.  There's not
;; sufficient reason to upgrade them to customize variables nor remove them as
;; it's easy to customize them in cases where it's necessary although this is
;; not expected to become useful.

;; TODO these can be used across buffers when set before cloning indirect
;; buffers, but that's a coincidence, not necessarilly a design choice.
(defvar-local ms-title nil
  "Presentation title.
If you have \"#+title:\" line in your org buffer, it wil be used
as a title of the slide.  If the buffer has no \"#+title:\" line,
the name of current buffer will be displayed.")

(defvar-local ms-email nil
  "Email address.
If you have \"#+email:\" line in your org buffer, it will be used
as an address of the slide.")

(defvar-local ms-author nil
  "Author name.
If you have \"#+author:\" line in your org buffer, it will be
used as a name of the slide author.")

(defvar-local ms-date nil
  "Date.
If you have \"#+date:\" line in your org buffer, it will be used
as the date.")

;; TODO make public
(defun ms--make-header (&optional no-breadcrumbs)
  "Draw a header for the first tree in the restriction.
Set optional NO-BREADCRUMBS to non-nil to skip breadcrumbs.  The implementation
assumes the buffer is restricted and that there is a first tree."
  (ms--delete-header)

  ;; Use of point-min is an implementation assumption, that the header is always
  ;; at the very top of the narrowed region and never wanted anywhere else.
  (setq ms--header-overlay
        (make-overlay (point-min) (+ 1 (point-min))))

  (let* ((blank-lines ms-content-margin-top)
         (keywords (org-collect-keywords
                    '("TITLE" "EMAIL" "AUTHOR" "DATE")))
         (title (or ms-title
                    (cadr (assoc-string "TITLE" keywords))
                    (buffer-name)))
         (author (or ms-author
                     (cadr (assoc "AUTHOR" keywords))))
         (date (or ms-date
                   (cadr (assoc-string "DATE" keywords))
                   (format-time-string "%Y-%m-%d")))
         (email (when-let ((email (or ms-email
                                      (cadr (assoc-string "EMAIL" keywords)))))
                  (concat "<" email ">"))))

    ;;  The calls to `propertize' make up for the fact that these values may be
    ;;  strings, set from elsewhere, but we want to display these strings as if
    ;;  they were fontified within the buffer.
    (if ms-header
        (overlay-put
         ms--header-overlay 'before-string
         (concat (propertize title 'face 'org-document-title)
                 (ms--info-face "\n")
                 (when (and  ms-header-date date)
                   (ms--info-face (concat date "  ")))
                 (when (and  ms-header-author author)
                   (ms--info-face (concat author "  ")))
                 (when (and  ms-header-email email)
                   (ms--info-face (concat email "  ")))
                 (when (and (not no-breadcrumbs)
                            ms-breadcrumb-separator)
                   (concat (ms--info-face "\n")
                           (ms--get-parents
                            ms-breadcrumb-separator)))
                 (ms--get-blank-lines blank-lines)))

      (overlay-put ms--header-overlay 'before-string
                   (ms--get-blank-lines blank-lines)))))

(defun ms--info-face (s)
  (propertize s 'face 'org-document-info))

(defun ms--get-blank-lines (lines)
  "Return breaks by LINES."
  (ms--info-face (make-string lines ?\12))) ; ?\12 is newline char

(defun ms--breadcrumbs-reducer (delim)
  (lambda (previous next)
    (if (not previous) next
      (let ((props (text-properties-at (1- (length previous)) previous)))
        (concat previous (apply #'propertize delim props)
                next)))))

;; TODO element API
(defun ms--get-parents (delim)
  "Get parent headings and concat them with DELIM."

  ;; The implementation here uses the regex & point-based techniques so that
  ;; we're extracting buffer strings, which saves us from having to re-style
  ;; them to match whatever is in the buffer.
  (save-excursion
    (goto-char (point-min))
    (save-restriction
      (widen)
      (let ((parents nil)
            (reducer (ms--breadcrumbs-reducer delim)))
        (while (org-up-heading-safe)
          (push (org-get-heading
                 'no-tags
                 ms-breadcrumbs-hide-todo-state)
                parents))
        (let ((breadcrumbs (seq-reduce reducer parents nil)))
          (when ms-breadcrumb-face
            (add-face-text-property 0 (length breadcrumbs)
                                    ms-breadcrumb-face
                                    nil
                                    breadcrumbs))
          breadcrumbs)))))

(defun ms--delete-header ()
  "Delete header."
  (when ms--header-overlay
    (delete-overlay ms--header-overlay)))

;; * Animation

(defvar ms--animation-timer nil)
(defvar-local ms--animation-overlay nil)

(defcustom ms-animation-duration 1.0
  "How long slide in takes."
  :type 'number
  :group 'macro-slides)

(defcustom ms-animation-frame-duration (/ 1.0 60.0)
  "Length between updates.
Increase if your so-called machine has trouble drawing."
  :type 'number
  :group 'macro-slides)

;; TODO move respect for animation variables into this function
;; TODO END is a redundant argument unless a virtual newline is introduced.
;; Test if an overlay can can work via after-string.
;; TODO Support non-graphical
(defun ms-animation-setup (beg end)
  "Slide in the region from BEG to END.
Everything after BEG will be animated.  The region between BEG
and the value of `point-max' should contain a newline somewhere."
  (unless (ms-live-p)
    (error "Slide animation attempted without active deck"))
  (unless (buffer-base-buffer (current-buffer))
    (error "Slide animation attempted in wrong buffer"))
  (ms--animation-cleanup)
  (let* ((timer (setq ms--animation-timer (timer-create)))
         (goal-time (time-add (current-time)
                              ms-animation-duration))
         (newline-region (save-match-data
                           (save-excursion
                             (goto-char beg)
                             (if (re-search-forward "\n" end t)
                                 (list (match-beginning 0)
                                       (match-end 0))
                               (error "No newline in region")))))
         (overlay (setq ms--animation-overlay
                        (apply #'make-overlay newline-region)))
         (initial-line-height
          (or (plist-get
               (text-properties-at (car newline-region))
               'line-height)
              1.0)))
    (timer-set-time timer (current-time)
                    ms-animation-frame-duration)
    (timer-set-function timer #'ms--animate
                        (list goal-time overlay initial-line-height))
    (timer-activate timer)))

;; * Assorted Implementation Details

;; TODO Watching actions, results, and slides is way too opaque
(defun ms--debug (slide)
  (when ms--debug
    (let* ((heading (ms-heading slide))
           (headline-begin (org-element-begin heading))
           (headline-end (or (org-element-contents-begin heading)
                             (org-element-end heading))))
      (message "begin: %s heading: %s"
               (marker-position (oref slide begin))
               (save-restriction
                 (widen)
                 (buffer-substring headline-begin (1- headline-end)))))))

(defun ms--clean-up-state ()
  "Clean up states between contents and slides."
  (ms--delete-header)
  (ms--delete-overlays)
  (ms--animation-cleanup))

(defun ms--ensure-deck ()
  "Prepare for starting the minor mode.
Call this when writing commands that could be called before or
after a deck exists but should create a deck if it does not exist.

In functions that should only be called when a deck is alive and
associated with the current buffer, use `ms-live-p'
and throw an error if it's not live.

This function sets up the deck and links the buffers together via
the deck object.  Many operations such as calling hooks must
occur in the display buffer."
  (unless (ms-live-p)
    ;; Prevent starting within indirect buffers
    (when (buffer-base-buffer (current-buffer))
      (error "Buffer is indirect but deck is already live"))

    ;; TODO check assumed initial conditions
    (let* ((base-buffer (current-buffer))
           (slide-buffer-name (format "*deck: %s*" (buffer-name
                                                    base-buffer))))
      (ms--feedback :start)

      ;; stale buffers likely indicate an issue
      (when-let ((stale-buffer (get-buffer slide-buffer-name)))
        (display-warning '(ms ms--ensure-deck)
                         "Stale deck buffer was killed")
        (kill-buffer slide-buffer-name))

      (let* ((class (or (intern-soft (ms--keyword-value
                                      "DECK_CLASS"))
                        ms-default-deck-class
                        'ms-deck))
             (window-config (current-window-configuration))

             (slide-buffer (clone-indirect-buffer
                            slide-buffer-name
                            nil))
             (deck (apply class
                          :base-buffer base-buffer
                          :slide-buffer slide-buffer
                          :window-config window-config
                          nil)))
        (setq ms--deck deck)
        (switch-to-buffer slide-buffer) ;; TODO display options?

        (widen)
        (org-fold-show-all)
        ;; Enter the state model
        (ms--choose-slide deck
                          ms-start-from)
        (ms--remap-faces t)))))

(defun ms--showing-contents-p ()
  "Return t if current buffer is displaying contents."
  (and ms--deck
       (eq (current-buffer) (oref ms--deck slide-buffer))
       (eq 'contents (oref ms--deck slide-buffer-state))))

(defun ms--showing-slides-p ()
  "Return t if current buffer is displaying contents."
  (and ms--deck
       (eq (current-buffer) (oref ms--deck slide-buffer))
       (eq 'slides (oref ms--deck slide-buffer-state))))

(defun ms--delete-overlays ()
  "Delete content overlays."
  (while ms--overlays
    (delete-overlay (pop ms--overlays))))

(defun ms--animate (goal-time overlay initial-line-height)
  (if (time-less-p goal-time (current-time))
      (ms--animation-cleanup)
    (let* ((diff (time-to-seconds (time-subtract goal-time (current-time))))
           (fraction (expt (/ diff ms-animation-duration) 5.0))
           (lines ms-slide-in-blank-lines)
           (line-height (* (+ initial-line-height lines)
                           fraction)))
      (overlay-put overlay 'line-height line-height))))

(defun ms--animation-cleanup ()
  (when ms--animation-timer
    (cancel-timer ms--animation-timer))
  (when ms--animation-overlay
    (delete-overlay ms--animation-overlay))
  (setq ms--animation-overlay nil
        ms--animation-timer nil))

(defun ms--ensure-slide-buffer (&optional display)
  "Run in commands that must run in the slide buffer."
  (unless (ms-live-p)
    (error "Live deck not found within buffer"))
  (if display
      (display-buffer (oref ms--deck slide-buffer))
    (set-buffer (oref ms--deck slide-buffer))))

(defun ms--keyword-value (key)
  "Get values like #+KEY from document keywords."
  (cadr (assoc-string key (org-collect-keywords `(,key)))))

(defun ms--feedback (key)
  "Explicit feedback for commands without visible side effects."
  (when-let ((feedback (plist-get ms-feedback-messages
                                  key)))
    (message "%s" feedback)))

;; TODO these could check for inheritance from some base class, which would save
;; people who write action names in the class property etc.
(defun ms--classes (class-names)
  "CLASS-NAMES is a string that might contain class names."
  (when class-names
    (let ((class-names (if (stringp class-names)
                           (string-split class-names)
                         class-names)))
      (cl-loop for name in class-names
               for symbol = (or (when (symbolp name) name)
                                (intern-soft name))
               if (get symbol 'cl--class)
               collect symbol
               else
               do (display-warning
                   '(ms
                     ms-class
                     ms-filter)
                   (format "Class name not a class: %s" name))))))

(defun ms--filter (filter-name)
  "FILTER-NAME is a string that might contain a filter name."
  (when-let ((symbol (or (when (symbolp filter-name)
                           filter-name)
                         (intern-soft filter-name))))
    (if (functionp symbol)
        symbol
      (display-warning
       '(ms
         ms-class
         ms-filter)
       (format "Filter name not a function: %s" filter-name)))))

(defun ms--parse-class-with-args (property-data)
  (unless (string= "nil" property-data)
    (let ((classes-with-args
           (ms--parse-classes-with-args property-data)))
      (prog1 (car classes-with-args)
        (unless (= 1 (length classes-with-args))
          (display-warning '(macro-slides)
                           (format "Only one classes allowed: %s"
                                   (cdr classes-with-args))))))))

(defun ms--keyword-symbol-p (string)
  (eq 0 (string-match-p ":\\(?:\\sw\\|\\s_\\)+$" string)))

(defun ms--parse-classes-with-args (property-data)
  ;; To support org's multiple-value properties, we want to parse a string that
  ;; looks like "class-name :arg val class-name :arg val :arg val", basically a
  ;; space-separated list of either class names or key-value pairs that are
  ;; arguments for those classes during instantiation.  The result is a form of
  ;; ((CLASS . ARGS)) where ARGS is a plist.
  (unless (string= "nil" property-data)
    (let ((tokens (split-string property-data))
          classes-with-args
          class-with-args)
      (condition-case err
          (while-let ((token (pop tokens))
                      (class (ms--class token t)))
            ;; peak for a key to decide if we continue parsing as args go back
            ;; to parsing as class names
            (push class class-with-args)
            (while-let ((token (car tokens))
                        (tokenp (ms--keyword-symbol-p token)))
              ;; TODO this could create new symbols?  Anyway, using `make-symbol'
              ;; is extremely ill-advised here ☢️ and `intern-soft' should work
              ;; since the class should already exist, but I didn't check on this.
              (push (intern (pop tokens)) class-with-args)
              (let ((val (pop tokens)))
                (push (car (read-from-string val)) class-with-args)))
            (push (reverse class-with-args) classes-with-args)
            (setq class-with-args nil))
        (wrong-type-argument
         (display-warning
          '(macro-slides) (cdr err))))
      (reverse classes-with-args))))

;; This should not interpret nil's specially because that should he handled
;; upstream by the parse functions
(defun ms--class (class-name &optional signal)
  "CLASS-NAME is a string or symbol that should be a class name.
Optional ERROR if you want to process `wrong-type-argument'."
  (let* ((symbol (or (when (symbolp class-name)
                       class-name)
                     (intern-soft class-name)))
         (class (when (get symbol 'cl--class) symbol)))
    (if (and class symbol)
        symbol
      (if signal
          (signal 'wrong-type-argument
                  (format "Class name not a class: %s" class-name))
        (display-warning
         '(macro-slides)
         (format "Class name not a class: %s" class-name))
        nil))))

;; TODO let's just move face remapping to master of ceremonies
(defun ms--remap-faces (status)
  "Change status of heading face.  If STATUS is nil, apply the default values."
  (cond
   (status
    (setq
     ms-heading-level-1-cookie
     (face-remap-add-relative 'org-level-1 'ms-heading-level-1)
     ms-heading-level-2-cookie
     (face-remap-add-relative 'org-level-2 'ms-heading-level-2)
     ms-heading-level-3-cookie
     (face-remap-add-relative 'org-level-3 'ms-heading-level-3)
     ms-heading-level-4-cookie
     (face-remap-add-relative 'org-level-4 'ms-heading-level-4)
     ms-heading-level-5-cookie
     (face-remap-add-relative 'org-level-5 'ms-heading-level-5)
     ms-heading-level-6-cookie
     (face-remap-add-relative 'org-level-6 'ms-heading-level-6)
     ms-heading-level-7-cookie
     (face-remap-add-relative 'org-level-7 'ms-heading-level-7)
     ms-heading-level-8-cookie
     (face-remap-add-relative 'org-level-8 'ms-heading-level-8)
     ms-document-title-cookie
     (face-remap-add-relative 'org-document-title
                              'ms-document-title)
     ms-document-info-cookie
     (face-remap-add-relative 'org-document-info
                              'ms-document-info)))
   (t
    (face-remap-remove-relative ms-heading-level-1-cookie)
    (face-remap-remove-relative ms-heading-level-2-cookie)
    (face-remap-remove-relative ms-heading-level-3-cookie)
    (face-remap-remove-relative ms-heading-level-4-cookie)
    (face-remap-remove-relative ms-heading-level-5-cookie)
    (face-remap-remove-relative ms-heading-level-6-cookie)
    (face-remap-remove-relative ms-heading-level-7-cookie)
    (face-remap-remove-relative ms-heading-level-8-cookie)
    (face-remap-remove-relative ms-document-title-cookie)
    (face-remap-remove-relative ms-document-info-cookie))))

(provide 'macro-slides)
