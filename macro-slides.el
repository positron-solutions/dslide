;;; macro-slides.el --- A presentation framework -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2011-2023 Takaaki ISHIKAWA
;; Copyright (C) 2024 Positron
;;
;; Author: Positron <contact@positron.solutions>
;; Version: 0.2.1
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
;;    org-mode 9.6.29 or higher version
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
;; 1. Class interface definitions for stateful sequence, deck (root sequence),
;; slide, and actions (sequences that run within slides).
;;
;; 2. Element mapping implementations that are private but exposed publicly on
;; slide actions and elsewhere because they are super useful.
;;
;; 3. Miscellaneous implementation details of parsing arguments, debug printing,
;; header, animation etc.
;;
;; 4. Lifecycle of the mode, switching between base buffer, contents, and
;; slides, user interface commands.
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
           :stop "Stop ■"
           :after-last-slide "No more slides!")
  "Feedback messages for slide controls.
Turn off by setting to nil.  Plist keys:
- :start `ms-start'
- :forward `ms-forward'
- :backward `ms-backward'
- :contents `ms-contents'
- :stop `ms-stop'
  :after-last-slide: see `after-last-slide' hook"
  :type 'plist
  :group 'macro-slides)

(defcustom ms-breadcrumb-face nil
  "Face added to the list of faces for breadcrumbs.
This can be a face name symbol or an anonymous font spec.  It
will be added to the face list, meaning it the original face's
properties remain unless shadowed."
  :type 'face
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

(defcustom ms-animation-duration 1.0
  "How long slide in takes."
  :type 'number
  :group 'macro-slides)

(defcustom ms-animation-frame-duration (/ 1.0 60.0)
  "Length between updates.
Increase if your so-called machine has trouble drawing."
  :type 'number
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

(defcustom ms-after-last-slide-hook '()
  "Run when forward is called but there is no next slide.
This can either provide feedback or quit immediately etc.
Consider using `ms-push-step' and writing a callback that only
reacts to the `forward' state.  This callback will then only run
if the user immediately calls `ms-forward' again.  `ms-stop' is
another good choice."
  :group 'macro-slides
  :type 'hook)

(defcustom ms-default-slide-action #'ms-action-narrow
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

(defcustom ms-default-section-actions '()
  "Actions that run within the section display action lifecycle.
It's value is a list of `ms-action' sub-classes or (CLASS . ARGS)
forms where ARGS is a plist.  Each subclass will be instantiated
into an action object.  See the `ms-action' class and its methods
to learn about writing custom actions.

Many section actions are no-op whenever the content doesn't
contain any elements they act on.  You can add classes to this
list in order to have default behaviors for some org elements.

You can configure this per-heading by setting the
MS_SECTION_ACTIONS keyword.  You can configure it for the
document default by adding an MS_SECTION_ACTIONS keyword."
  :type '(list function)
  :group 'macro-slides)

(defcustom ms-default-child-action #'ms-child-action-slide
  "Action run after section lifecycle.
Value is an action class, usually extending
`ms-child-action'.  The usual purpose is to manage
the child headings, which come after the section element.

You can configure this per-heading by setting the
MS_CHILD_ACTION keyword.  You can configure it for the
document default by adding an MS_CHILD_ACTION keyword."
  :type 'function
  :group 'macro-slides)

(defcustom ms-default-class 'ms-slide
  "A class to more deeply modify slide behavior.
Value should be a custom class extending `ms'.  You
can override methods if the built-in implementation is
insufficient.  Consider upstreaming changes.

You can configure this per heading by setting the MS_CLASS
property.  You can configure it for the document default by
adding an MS_CLASS keyword."
  :type 'symbol
  :group 'macro-slides)

(defcustom ms-default-deck-class 'ms-deck
  "A class to more deeply modify overall deck behavior.
Value should be a custom class extending `ms-deck'.
Use this to modify the root-level behaviors, including switching
to children and finding siblings.  You can configure this for the
document by adding the MS_ROOT_CLASS keyword."
  :type 'symbol
  :group 'macro-slides)

(defcustom ms-default-filter #'ms-built-in-filter
  "A function used to call next on children.
The function used as actions should accept an org element, a
`headline' type element and return it if it is a valid heading or
return nil if it should be skipped.

You can configure this per heading by setting the MS_FILTER
keyword.  You can configure it for the document default by adding
an MS_FILTER keyword."
  :type 'function
  :group 'macro-slides)

(defcustom ms-contents-selection-highlight t
  "Show a highlight on the selected headline.
This is useful if you have some subtle cursor feature enabled for
your presentation and wouldn't otherwise know what line you are
on in the contents view.  The default is also just a way more
obvious display style."
  :type 'boolean
  :group 'macro-slides)

(defface ms-contents-selection-face
  '((t :inherit org-level-1 :inverse-video t :extend t))
  "Face for highlighting the current slide root."
  :group 'macro-slides)

(defface ms-highlight
  '((t :inherit hl-line))
  "Face used when following in the base buffer.
See `ms-base-follows-slide'."
  :group 'macro-slides)

(defvar ms--debug nil
  "Set to t for logging slides and actions.")


(defvar ms--animation-timer nil)
(defvar-local ms--animation-overlay nil)

;; Tell the compiler that these variables exist
(defvar ms-mode)

(defvar ms--deck nil
  "Active deck object.
This is global.  If a presentation is active, you can look at this variable to
coordinate with it.")

(defvar-local ms--overlays nil
  "Overlays used to hide or change contents display.")

(defvar-local ms--header-overlay nil
  "Flag to check the status of overlay for a slide header.")

;; Shouldn't need one per buffer
(defvar ms--contents-hl-line-overlay nil
  "Highlights selected heading in contents view.")

(defconst ms--display-actions
  '(display-buffer-same-window display-buffer-in-previous-window)
  "Configure `display-buffer-alist' to override.")

;; * Classes

;; This generic functions below are the most important interfaces for all
;; hacking of this package.
;;
;; The domain model first must describe a linear sequence of steps that the user
;; traverses both forward and backward.
;;
;; There are some states that may need to be set up or torn down at the
;; boundaries of the sequence.  These are handled by three methods, init, end,
;; and final.
;;
;; End is essentially init for going in reverse.  Usually this is the same as
;; calling init and then stepping forward until no more progress is made.
;; However doing it this way would be unable to avoid extra work and could even
;; create headaches when implementing sequences that shouldn't use reverse to
;; un-execute the forwards steps or in cases where implementing this is too
;; complex to pay off to the user.  For these reasons, the implementation of
;; `ms-end' is left up to the user.
;;
;; Goto essentially is just a careful use of step-forward.  If every forward
;; step properly reports its maximum extent of progress, we can use forward and
;; init to implement every goto.
;;
;; Finally, step-forward and step-backward should navigate the states between
;; init / end and final.  They just return non-nil until they are done.  The
;; caller doesn't care about the implementation, and that is why EIEIO is used.
;;
;; Sub-sequences can rely on the parent state to exist for their entire
;; lifetime. The parent sequence will not call its own `ms-final' until after it
;; has called the sub-sequence's `ms-final'.
;;
;; Sub-sequences currently don't have any first-class extensible support for
;; entering or exiting the sub-sequence.  Such cooperation is present in limited
;; amounts to limit coupling the parent and child sequences.
;;
;; A lazy implementer can forego methods by delegating them to simpler
;; idempotent methods, such as using an idempotent init for step-backward.  With
;; a maximum of six methods and a minimum of two, just init and forward, you
;; have enough behavior to properly fit the user interface.

(cl-defgeneric ms-init (obj)
  "Called when entering a sequence.
Set up the state required for this sequence when going forward,
entering the sequence from the beginning.

Return values are ignored.  `ms-init' always counts as a step
because it's a result of a nil return from `ms-forward'.

This method should work together with `ms-end' and `ms-final' to
ensure consistently valid state for `ms-forward' and
`ms-backward'.")

(cl-defgeneric ms-end (obj)
  "Init when going backwards.
Set up the state required for this sequence when going backward,
entering the sequence from the end.

Return values are ignored.  `ms-end' always counts as a step
because it's a result of a nil return from `ms-backward'.

The first job of this method is to perform setup, possibly by
just calling init since they likely have similar side-effects.

Second, this method should reach the state that is equivalent to
if the user called forward until no more progress could be made.

The default implementation calls `ms-init' and then calls
`ms-step-forward' until no more progress can be made.  If this is
inappropriate, it should be overridden.

In cases where you don't need a real backward implementation or
progressing backwards would have no sensible behavior, you can
delegate this to `ms-init' and possibly delegate `ms-backward' to
`ms-forward', resulting in a sequence that always starts at the
beginning and always proceeds to the end.  For a single step
sequence that has identical effect in both directions, this is
appropriate.

This method should work together with `ms-end' and `ms-final' to
ensure consistently valid state for `ms-forward' and
`ms-backward'")

(cl-defgeneric ms-final (obj)
  "Called when exiting a sequence.
Implement this method to clean up any state that would interfere
with the sequence succeeding when run again.  If your sequence
implements real backward behavior,

All side-effects and states created by steps in the sequence or
the `ms-init' and `ms-end' methods must be cleaned up or
otherwise managed or else `ms-step-backward' and other sequences
of running a presentation will be brittle and likely fail when
re-run.")

(cl-defgeneric ms-step-forward (obj)
  "Make one step forward.
The return value has meaning to the deck:

- t: progress was made

- a point: progress was made up to a specific buffer location

- nil: no progress could be made.

For sequences that don't make progress in a buffer, returning t
is fine.  Returning a point of progress is necessary for the
default implementation of `ms-goto'.

⚠ Every sequence of `ms-step-forward' should return nil at some
point or else infinite loops will result.")

(cl-defgeneric ms-step-backward (obj)
  "Make one step backwards and return earliest point.
The return value has meaning to the deck:

- t: progress was made

- a point: progress was made up to a specific buffer location

- nil: no progress could be made.

For sequences that don't make progress in a buffer, returning t
is fine.  Returning a point of progress is necessary for the
default implementation of `ms-goto'.

⚠ Every sequence of `ms-step-backward' should return nil at some
point or else infinite loops will result.")

(cl-defgeneric ms-goto (obj point)
  "Step forward until advancing beyond POINT.
This method can usually be implemented on top of
`ms-step-forward' by advancing until POINT is exceeded.  Return
nil if POINT was not exceeded.  Return non-nil if the sense of
progress exceeds POINT.  Usually, child actions will be
responsible for determining if the POINT belongs to this slide or
one of its child slides, and the slide will just ask the child
action.")

;; ** Stateful Sequence
(defclass ms-stateful-sequence ()
  ((parent
    :initval nil
    :initarg :parent
    :documentation "Parent or root sequence.
Usually a deck or slide.  In the function stack analogy, this is
the same as storing a stack pointer for returning to the caller."))

  "An interface definition for linear sequences of steps.
This is an abstract class.

The sequence can be traversed forwards and backward.  `init' and
`foward' are conjugates of `end' and 'backward'.

Because the sequence steps may rely on some setup and should
perform necessary teardown, the stateful sequence provides `init'
`end' and `final' methods.

It can also be indexed by high-level navigation commands.  The
implementation of `ms-goto' Sequences can run as sub-sequences,
where one sequence calls into another.

Classes that wish to implement the stateful sequence interface
just need to support a few methods and then rely on the generic
implementations for the rest, unless they want to optimize or
simplify their implementation."
  :abstract t)

(cl-defmethod ms-init ((_ ms-stateful-sequence)))

(cl-defmethod ms-end ((obj ms-stateful-sequence))
  (let ((progress t))
    (while progress
      (setq progress (ms-step-forward obj)))))

(cl-defmethod ms-step-forward ((_ ms-stateful-sequence)))

(cl-defmethod ms-step-backward ((_ ms-stateful-sequence)))

(cl-defmethod ms-final ((_ ms-stateful-sequence)))

(cl-defmethod ms-goto ((obj ms-stateful-sequence) point)
  (unless (eq 'skip (ms-init obj))
    (let (exceeded (advanced t))
      (while (and advanced (not exceeded))
        (let ((progress (ms-step-forward obj)))
          (if (and (numberp progress)
                   (>= progress point))
              (setq exceeded t)
            (setq advanced progress)))))))

;; ** Progress
(defclass ms-progress-tracking ()
  ((marker
    :initform nil
    :initarg :marker
    :documentation "Marker used to track progress"))
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

;; ** Parent
;; TODO this class is kind of half-baked.  It was intended to wrap up the
;; filtering functionality and needing to find next and previous children.
;; Needs actual usage to become mature.
(defclass ms-parent ()
  ((filter
    :initform nil
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
(defclass ms-deck (ms-progress-tracking ms-parent)
  ((slide
    :initform nil
    :documentation "The active sequence or slide.
This is probably a `ms-slide' object, but anything
that implements `ms-stateful-sequence' will probably
work as well.")
   (base-buffer
    :initform nil :initarg :base-buffer
    :documentation "Source of the slide deck.")
   (slide-buffer
    :initform nil :initarg :slide-buffer
    :documentation "Indirect buffer used to display slides in.")
   (window-config
    :initform nil :initarg :window-config
    :documentation"Window configuration for restoring after stop.")
   ;; TODO this implementation doesn't work if more indirect buffers are used.
   (slide-buffer-state
    :initform nil
    :documentation "Initiated by display actions to `contents' or `slides'.")
   (step-callbacks
    :initform nil
    :documentation "Steps to run before next steps.
FORM is just a list as steps will always be run before any
sequence ends or makes progress.."))

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

  ;; TODO This line is critical to starting up the state machine.  Slides
  ;; are still inferring their need to narrow.
  (narrow-to-region (point) (point)) ; signal to slide to draw itself
  (ms-init (oref obj slide)))

(cl-defmethod ms-end ((_ ms-deck))
  (error "Deck has no valid concept of starting at the end."))

(cl-defmethod ms-final ((obj ms-deck))
  (when-let ((slide (oref obj slide)))
    (ms-final slide)))

;; Deck forward & backward methods are the entry point for user forward and
;; backward commands.  They delegate out to slides, which may telescope into
;; their children in order to make progress.
;;
;; It make require several trips through the behavior to consume callbacks that
;; are run for effect or are no-op, things that don't count as steps or are
;; slides that decide at runtime to be skipped.
;;
;; There are many little user-facing behaviors, such as following the slide in
;; the base buffer with the point.  These are best done from the sequence root.
;; It bloats the function, but has little effect on the complexity of the logic.
;;
;; So that sounds like a lot, but it's really simple.  Loop through whatever
;; next steps and callbacks were pushed onto the stack.  When one of them makes
;; progress, we're done.

(cl-defmethod ms-step-forward ((obj ms-deck))
  (unless (oref obj slide)
    ;; Calls implied from other commands should have started the lifecycle
    ;; already
    (error "No slide selected"))

  (let (progress reached-end)
    ;; Burn up a step callback until one returns non-nil
    (when-let ((steps (oref obj step-callbacks)))
      (while (and (not progress)
                  steps)
        (setq progress (funcall (pop steps) 'forward)))
      (oset obj step-callbacks steps))

    (while (not (or progress reached-end))
      (let* ((current-slide (oref obj slide))
             (result (ms-step-forward current-slide))
             next-slide)

        (if result
            (setq progress result)
          ;; Check if there is a next sibling.
          (if-let ((next-child (ms-next-child obj current-slide)))
              (setq next-slide next-child)
            (setq reached-end t)))

        (unless next-slide
          (ms--debug current-slide (format "forward: %s" progress)))

        (when next-slide
          (ms--debug next-slide "switching to sibling")
          (oset obj slide next-slide)
          (ms-final current-slide)

          (ms-init next-slide)
          ;; Init counts as a step
          (setq progress next-slide))))

    ;; A lot of progress may have happened, but there will be only one feedback
    ;; message.
    (when progress
      (ms--feedback :forward)
      (ms--follow progress))

    (when reached-end
      (ms--feedback :after-last-slide)
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

  (let (progress reached-beginning)
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
             previous-slide)

        (if result
            (setq progress result)
          ;; Check if there is a previous sibling.
          (if-let ((previous-child (ms-previous-child
                                    obj current-slide)))
              (setq previous-slide previous-child)
            (setq reached-beginning t)))

        (unless previous-slide
          (ms--debug current-slide (format "forward: %s" progress)))

        (when previous-slide
          (ms--debug previous-slide "switching to sibling")
          (oset obj slide previous-slide)
          (ms-final current-slide)

          ;; end counts as a step.
          (ms-end previous-slide)
          (setq progress previous-slide))))

    ;; A lot of progress may have happened, but there will be only one feedback
    ;; message.
    (cond (progress
           (ms--feedback :backward)
           (ms--follow progress))
          (reached-beginning
           (user-error "No more previous slides!")))))

(cl-defmethod ms--choose-slide ((obj ms-deck) how)
  "Set the current slide, according to HOW.
Optional POINT allows resolving a slide by walking the tree to
find the slide that displays that POINT."
  ;; TODO apply filter when choosing starting slide
  (cond ((eq how 'first)
         (oset obj slide (ms--make-slide
                          (ms--document-first-heading) obj)))
        ((eq how 'contents)
         (oset obj slide (ms--make-slide
                          (ms--root-heading-at-point (point))
                          obj)))
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

(defun ms-push-window-config (&optional step)
  "Save the window configuration and narrowing for restoration.

Optional STEP argument will decide if the callback counts as a step or will
return nil so that it is only run for effects."
  (let ((window-config (current-window-configuration)))
    (ms-push-step
     (lambda (_) (prog1 step
              (set-window-configuration window-config))))))

(defun ms-push-step (fun)
  "Run FUN as next step.
FUN is a function of a single optional argument, `forward' or
`backward'.  nil indicates that the callback is being cleaned up,
usually to quit the presentation or change to contents.

The return value is interpreted as progress, so return non-nil if
you want FUN to count as a step or nil if FUN is only run for
effects.

If you need multiple steps, consider adding steps inside of FUN
for recursive dynamic steps rather than adding a lot of steps at
once, which requires the functions to be removed or return nil."
  (unless (ms-live-p)
    (error "No active deck"))
  (push fun (oref ms--deck step-callbacks)))

;; * Slide
(defclass ms-slide (ms-parent ms-stateful-sequence)
  ((slide-action
    :initform nil :initarg :slide-action
    :documentation "Action run around both section and child actions.
See `ms-default-slide-action'.")
   (section-actions
    :initform nil :initarg :section-actions
    :documentation "Typical actions that work on the section.
Live within slide action lifecycle.  See
`ms-default-section-actions'.")
   (child-action
    :initform nil :initarg :child-action
    :documentation "Action run after section.
Live within slide action lifecycle. See
`ms-default-child-action'.")
   (begin
    :initform nil :initarg :begin
    :documentation "Marker for retrieving this heading's org element."))

  "Slides store some local state and delegate behavior to several
functions. The Slide is a stateful node that hydrates around a
heading and stores actions and their states.")

(cl-defmethod ms-init ((obj ms-slide))
  (when-let ((slide-action (oref obj slide-action)))
    (ms-init slide-action))
  (when-let ((section-actions (oref obj section-actions)))
    (mapc #'ms-init section-actions))
  (when-let ((child-action (oref obj child-action)))
    (ms-init child-action)))

(cl-defmethod ms-end ((obj ms-slide))
  (when-let ((slide-action (oref obj slide-action)))
    (ms-end slide-action))
  ;; Fairly certain the ordering of child and section actions doesn't actually
  ;; matter for `ms-end', but this ordering matches the situation that would
  ;; occur if the user just called `ms-step-forward' repeatedly, and we want the
  ;; end state to be as close to "normal" as possible.
  (when-let ((section-actions (oref obj section-actions)))
    (mapc #'ms-end (reverse section-actions)))
  (when-let ((child-action (oref obj child-action)))
    (ms-end child-action)))

(cl-defmethod ms-final ((obj ms-slide))
  ;; The order that these are called shouldn't matter.  No use case for coupling
  ;; different finals, but the guarantee is that the lifecycle of the slide
  ;; actions encompass the contents actions (child and section)
  (mapc (lambda (action)
          (ms-final action))
        (oref obj section-actions))
  (when-let ((child-action (oref obj child-action)))
    (ms-final child-action))
  (when-let ((display-action (oref obj slide-action)))
    (ms-final display-action))
  ;; Clean up heading marker, which is shared by children
  (set-marker (oref obj begin) nil))

(cl-defmethod ms-step-forward ((obj ms-slide))
  (let ((section-actions (oref obj section-actions))
        (child-action (oref obj child-action))
        (slide-action (oref obj slide-action))
        progress)
    (while (and (not progress) section-actions)
      (setq progress (ms-step-forward (pop section-actions))))
    (unless (or progress (null child-action))
      (setq progress (ms-step-forward child-action)))
    (unless (or progress (null slide-action))
      (setq progress (ms-step-forward slide-action)))
    progress))

(cl-defmethod ms-step-backward ((obj ms-slide))
  (let ((section-actions (oref obj section-actions))
        (child-action (oref obj child-action))
        (slide-action (oref obj slide-action))
        progress)
    (unless (null child-action)
      (setq progress (ms-step-backward child-action)))
    (while (and (not progress) section-actions)
      (setq progress (ms-step-backward (pop section-actions))))
    (unless (or progress (null slide-action))
      (setq progress (ms-step-backward slide-action)))
    progress))

;; `ms--make-slide' is very critical to the user-facing configuration and
;; hacker-facing capabilities and API.  Slides are hydrated from org mode
;; headings.  We can pretty much divide the likely user needs into either what
;; to do with the section and what to do with the child headings.

;; Because the section needs to be narrowed to, and this narrowing must be
;; performed both forwards and backwards, we also have a slide action that runs
;; in very particularly ordered points to keep its operation simple and reliable.
;;
;; There is a chance that it will make sense to support nested s-expressions in
;; the property configuration.  For now, there is only an observed need for
;; configuring either the section action or just the child action.  A property
;; configuration API that supports nesting is not expected to look that
;; different.  It will involve a bit more parsing.
;;
;; Both child actions and user configuration have demonstrated a large benefit
;; from being able to slightly change the behavior of actions.  This is why
;; `ms--make-slide' supports plist arguments when hydrating from org properties
;; and why child actions that create slides can pass these in via `args'.

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
  (let* ((begin-position (org-element-property :begin heading))
         (begin (make-marker))
         (inline (plist-get args :inline))
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
                      '("MS_SLIDE_ACTION"
                        "MS_SECTION_ACTIONS"
                        "MS_CHILD_ACTION"
                        "MS_FILTER"
                        "MS_CLASS")))

           (args `(:inline ,inline))

           ;; TODO just munged this a bit for explicit nil handling.  Might
           ;; still have precedence wrong.  If there is any string set in any
           ;; property, the default value shouldn't be used.
           (slide-action-class
            (or slide-action-class
                (if-let ((declared
                          (or (org-element-property :MS_SLIDE_ACTION heading)
                              (cdr (assoc-string "MS_SLIDE_ACTION"
                                                 keywords)))))
                    (ms--parse-class-with-args declared)
                  ms-default-slide-action)))

           ;; TODO precedences are out of wack.  Heading property should win
           ;; versus child heading, document, or default
           (slide-action (when slide-action-class
                           (if (consp slide-action-class)
                               (apply (car slide-action-class)
                                      :begin begin
                                      (append args
                                              slide-action-args
                                              (cdr slide-action-class)))
                             (apply slide-action-class
                                    :begin begin
                                    (append args
                                            slide-action-args)))))

           ;; TODO action arguments might make sense, such as telling nested
           ;; elements not to animate.  It's really hard for them to infer being
           ;; in an inline child versus an independent slide, even by looking at
           ;; the restriction.
           (section-action-classes
            (or (ms--parse-classes-with-args
                 (or (org-element-property :MS_SECTION_ACTIONS heading)
                     (cdr (assoc-string "MS_SECTION_ACTIONS" keywords))))
                ms-default-section-actions))
           (section-actions
            (mapcar
             (lambda (c) (when c
                      (if (consp c)
                          (apply (car c) :begin begin
                                 (append args (cdr c)))
                        (apply c :begin begin args))))
             section-action-classes))

           ;; TODO Likely some precedence funk here.  Copied from above.
           (child-action-class
            (or child-action-class
                (if-let ((declared
                          (or (org-element-property :MS_CHILD_ACTION heading)
                              (cdr (assoc-string "MS_CHILD_ACTION"
                                                 keywords)))))
                    (ms--parse-class-with-args declared)
                  ms-default-child-action)))

           (child-action (when (and child-action-class
                                    (not (eq child-action-class 'none)))
                           (if (consp child-action-class)
                               (apply (car child-action-class)
                                      :begin begin
                                      (append args
                                              child-action-args
                                              (cdr child-action-class)))
                             (apply child-action-class
                                    :begin begin
                                    (append
                                     args
                                     child-action-args)))))

           (filter
            (or (ms--filter
                 (or (org-element-property :MS_FILTER heading)
                     (cdr (assoc-string "MS_FILTER" keywords))))
                ms-default-filter))
           (class
            (or (ms--parse-class-with-args
                 (or (org-element-property :MS_CLASS heading)
                     (cdr (assoc-string "MS_CLASS"
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
(defclass ms-action (ms-stateful-sequence ms-progress-tracking)
  ((begin
    :initform nil :initarg :begin
    :documentation "Marker for beginning of heading.  Used to
re-hydrate the org element for use in mapping over the section etc.")
   (inline
     :initform nil
     :initarg :inline
     :docuemntation "Draw as if surrounded by other contents.
This option allows actions that perform some animation to degrade
to some technique that works with contents above and below."))
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
        (ms-marker obj (org-element-property :begin next)))
    (ms-marker obj (org-element-property :end (ms-heading obj)))
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
        (ms-marker obj (org-element-property :begin previous)))
    (ms-marker obj (org-element-property :begin (ms-heading obj)))
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
  (ms-marker obj (org-element-property :begin (ms-heading obj))))

(cl-defmethod ms-end ((obj ms-action))
  (ms-marker obj (org-element-property :end (ms-heading obj))))

(cl-defmethod ms-final ((obj ms-action))
  (when-let ((marker (oref obj marker)))
    (set-marker marker nil)))

;; ** Default Slide Action
(defclass ms-action-narrow (ms-action)
  ((include-restriction
    :initform nil :initarg :include-restriction
    :documentation "Include the existing restriction.")
   (with-children
    :initform nil :initarg :with-children
    :documentation "Narrow should include children.
The default, nil, narrows to the section only."))
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
                  (org-element-property :end heading)
                (ms--section-end heading))))

    (if (oref obj include-restriction)
        (unless (and (<= (point-min) begin)
                     (>= (point-max) end))
          (narrow-to-region (min (point-min) begin)
                            (max (point-max) end))
          (run-hooks 'ms-narrow-hook)
          (when ms-slide-in-effect
            (ms-animation-setup begin end))
          (setq progress begin))
      (unless (and (<= (point-min) begin)
                   (>= (point-max) end))
        ;; TODO overlay-based display
        (narrow-to-region begin end)
        (run-hooks 'ms-narrow-hook)
        (ms--make-header)
        (goto-char (point-min))         ; necessary to reset the scroll
        (when (and ms-slide-in-effect
                   (not (oref obj inline)))
          (ms-animation-setup begin end))
        (setq progress begin)))
    ;; Return progress to count as step when re-narrowing after a child.
    progress))

;; This code makes little sense.  See the slide's current ordering of calling
;; the slide action, and the reason will make sense.  A re-write will probably
;; get it right.  The key thing to note is that a parent can't re-display itself
;; unless it's going backwards.  It needs to display itself during end even
;; though the end of its children may clobber it.  This works, just awkwardly.
(cl-defmethod ms-init :after ((obj ms-action-narrow))
  (ms-narrow obj))

(cl-defmethod ms-step-forward ((_ ms-action-narrow)) ; odd
  nil)

(cl-defmethod ms-step-backward ((obj ms-action-narrow))
  (ms-narrow obj))

(cl-defmethod ms-end :after ((obj ms-action-narrow))
  (ms-narrow obj))

;; ** Reveal items section action
(defclass ms-action-item-reveal (ms-action)
  ((overlays
    :initform nil))
  "Hide all items and then reveal them one by one.")

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
              (end (overlay-end first))
              (start (overlay-start first)))
    ;; TODO We can let-bind animations false for child slides.
    ;; Or handle this via arguments in child actions
    (when ms-slide-in-effect
      (ms-animation-setup
       (overlay-start first) (overlay-end first)))
    (delete-overlay first)
    (oset obj overlays (cdr overlays))
    (ms-marker obj end)
    ;; return progress
    start))

(cl-defmethod ms-step-backward ((obj ms-action-item-reveal))
  (when-let ((previous-item (ms-section-previous obj 'item)))
    (oset obj overlays
          (cons (ms-hide-element previous-item)
                (and (slot-boundp obj 'overlays)
                     (oref obj overlays))))
    (org-element-property :begin previous-item)))

;; ** Babel Action

;; TODO automatically map the blocks during init and remove results... this is
;; kind of implemented but seems to inconsistently work.
;; TODO configure results removal behavior with an argument
;; TODO any display jank concerns due to results?  Possibly inhibit re-display.
;; TODO integrate with skipping with init and end.
(defclass ms-action-babel (ms-action)
  () "Execute source blocks as steps.
By default blocks execute one by one with step-forward.  You can mark a block to
be special with the keyword:

- #+attr_methods: init

- #+attr_methods: step-forward

- #+attr_methods: step-backward

- #+attr_methods: step-both

- #+attr_methods: end

- #+attr_methods: final

Other than step-both, which executes in either step direction,
these keywords correspond to the normal methods of the stateful
sequence class.  Blocks with method init, end, and final are all
executed during the corresponding method and do not count as
steps.")

(cl-defmethod ms--clear-results ((obj ms-action-babel))
  (without-restriction
    (ms-section-map
     obj 'src-block
     (lambda (e)
       (save-excursion
         (goto-char (org-element-property :begin e))
         (org-babel-remove-result-one-or-many nil))))))

(defun ms--method-block-pred
    (method-names &optional unnamed)
  "Return a predicate to match the METHOD-NAME.
Optional UNNAMED will return unnamed blocks as well."
  (lambda (block)
    (if-let* ((all-names (car (org-element-property
                               :attr_methods block)))
              (names (string-split all-names)))
        (when (seq-intersection method-names names)
          block)
      (when unnamed
        block))))

(defun ms--block-execute (block-element)
  (without-restriction
    (save-excursion
      ;; TODO catch signals provide user feedback & options to navigate to the
      ;; failed block.
      (goto-char (org-element-property :begin block-element))
      ;; Executing babel seems to widen and also creates messages, and this
      ;; results in flashing.  The downside of just inhibiting re-display until
      ;; after the call is that if re-display is needed, such as if calling
      ;; `sleep-for' in a loop, then no updates will be visible.  However, the
      ;; user should really handle this with a timer or process output and
      ;; process sentinel etc.
      (let ((inhibit-redisplay t))
        ;; t for don't cache.  We likely want effects
        (org-babel-execute-src-block t)))))

(cl-defmethod ms--get-blocks ((obj ms-action-babel) &optional method-name)
  "Return the block with keyword value METHOD-NAME.
The keywords look like:

#+attr_methods: METHOD-NAME

The possible values for METHOD-NAME correspond to the
stateful-sequence class methods.  METHOD-NAME is a string."
  (let ((predicate (ms--method-block-pred (list method-name))))
    (ms-section-map obj 'src-block predicate)))

(cl-defmethod ms-step-forward ((obj ms-action-babel))
  (when-let* ((predicate (ms--method-block-pred
                          '("step-forward" "step-both") t))
              (next (ms-section-next obj 'src-block predicate)))
    (ms--block-execute next)
    (org-element-property :begin next)))

(cl-defmethod ms-step-backward ((obj ms-action-babel))
  (when-let* ((predicate (ms--method-block-pred
                          '("step-backward" "step-both")))
              (prev (ms-section-previous obj 'src-block predicate)))
    (ms--block-execute prev)
    (org-element-property :begin prev)))

(cl-defmethod ms-init :after ((obj ms-action-babel))
  (when-let ((block-elements (ms--get-blocks obj "init")))
    (mapc #'ms--block-execute block-elements)))

(cl-defmethod ms-end :after ((obj ms-action-babel))
  (when-let ((block-elements (ms--get-blocks obj "end")))
    (mapc #'ms--block-execute block-elements)))

(cl-defmethod ms-final :after ((obj ms-action-babel))
  (when-let ((block-elements (ms--get-blocks obj "final")))
    (mapc #'ms--block-execute block-elements)))

;; ** Image Action

(defclass ms-action-image (ms-action)
  ((kill-buffer
    :initform nil
    :initarg :kill-buffer
    :documentation "Kill the buffer.  Default nil just buries it.")
   (include-linked
    :initform t
    :initarg :include-linked
    :documentation "Loads linked images.  See `org-display-inline-images'.")
   (refresh
    :initform nil
    :initarg :refresh
    :documentation "Reload images.  See `org-display-inline-images'."))
  "Show images fullscreen in a buffer.")

(cl-defmethod ms-init :after ((obj ms-action-image))
  (org-display-inline-images
   (oref obj include-linked)
   (oref obj refresh)
   (org-element-property :begin (ms-heading obj))
   (org-element-property :end (ms-heading obj))))

;; TODO implementation relies on org link opening.  Does not check for file or
;; check that image mode displays the link correctly.
;; TODO make it just a link action?
(cl-defmethod ms-step-forward ((obj ms-action-image))
  (when-let ((link (ms-section-next obj 'link)))
    (ms-push-window-config nil)

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
    (org-element-property :begin link)))

(cl-defmethod ms-step-backward ((obj ms-action-image))
  (when-let ((link (ms-section-previous obj 'link)))
    (ms-push-window-config nil)

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
    (org-element-property :begin link)))

;; * Child Actions
(defclass ms-child-action (ms-action) ()
  "Base class for child actions."
  :abstract t)

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
                          (> (org-element-property :begin child) marker)
                          child))
                   nil t)))
      (prog1 next
        (ms-marker obj (org-element-property :begin next)))
    (ms-marker obj (org-element-property :end (ms-heading obj)))
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
                            (< (org-element-property :begin child) marker)
                            child)))))))
      (prog1 next
        (ms-marker obj (org-element-property :begin next)))
    (ms-marker obj (org-element-property :begin (ms-heading obj)))
    nil))

;; ** Default Child Action
(defclass ms-child-action-slide (ms-child-action)
  ((child
    :initform nil
    :documentation "Current child."))
  "Default child action.  Children are independent slides.")

(cl-defmethod ms-step-forward ((obj ms-child-action-slide))
  ;; For child slides, we make a slide out of the next child heading and advance
  ;; our progress forward to the end of that child
  (let (progress)
    (when-let ((child (oref obj child)))
      (setq progress (ms-step-forward child))
      (unless progress
        (ms-final child)
        (oset obj child nil)))
    (unless progress
      (when-let ((child (ms-forward-child obj)))
        ;; TODO transitive action customization
        (let ((child (ms--make-slide child (oref ms--deck slide))))
          (ms-init child)
          (oset obj child child))
        (setq progress (org-element-property :begin child))))
    progress))

(cl-defmethod ms-step-backward ((obj ms-child-action-slide))
  ;; For child slides, we make a slide out of the previous child heading and
  ;; advance our progress backward to the beginning of that child
  (let (progress)
    (when-let ((child (oref obj child)))
      (setq progress (ms-step-backward child))
      (unless progress
        (ms-final child)
        (oset obj child nil)))
    (unless progress
      (when-let ((child (ms-backward-child obj)))
        ;; TODO transitive action customization
        (let ((child (ms--make-slide child (oref ms--deck slide))))
          (ms-end child)
          (oset obj child child))
        (setq progress (org-element-property :begin child))))
    progress))

(cl-defmethod ms-end :after ((obj ms-child-action-slide))
  (when-let ((child (ms-backward-child obj)))
    (let ((child (ms--make-slide child (oref ms--deck slide))))
      (prog1 (ms-end child)
        (oset obj child child)))))

(cl-defmethod ms-final :after ((obj ms-child-action-slide))
  (when-let ((child (oref obj child)))
    (ms-final child)))

;; ** Inline Child Action
;; While the basics of making a child out of the next heading are the same, an
;; action that controls children on its own does not return them to the deck.
;; It needs to update the buffer restriction as necessary, call lifecycle
;; functions, and pass through calls to step forward.

;; TODO round-robin child action
;; TODO every-child action

;; TODO override the child's own child action
(defclass ms-child-action-inline (ms-child-action)
  ((children
    :initform nil
    :documentation "Children that have been instantiated."))
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
                          :inline t
                          ;; TODO this won't compose at all
                          :slide-action-args '(:include-restriction t :with-children t)
                          :child-action 'none)))
            (progn (ms-init child)
                   (setq progress child)
                   (push child (oref obj children)))
          (setq exhausted t))))
    progress))

(cl-defmethod ms-step-backward ((obj ms-child-action-inline))
  (let (progress)
    (while (and (oref obj children) (not progress))
      ;; First try the most recently added child
      (setq progress (when-let* ((child (car (oref obj children))))
                       (ms-step-backward child)))

      ;; If the child didn't make progress, narrow it away
      (unless progress
        (let* ((finished (pop (oref obj children)))
               (heading (ms-heading finished)))
          (ms-backward-child obj)       ; for marker effects 💡
          ;; TODO do this with overlays in a nested child ☢️
          (when heading
            (narrow-to-region (point-min) (org-element-property :begin heading))
            (run-hooks 'ms-narrow-hook))
          (ms-final finished)
          (setq progress (car (oref obj children))))))
    progress))

(cl-defmethod ms-end :after ((obj ms-child-action-inline))
  (ms-marker obj (org-element-property :begin (ms-heading obj)))
  (let (exhausted)
    (while (not exhausted)
      ;; If the child didn't make progress, try to load up the next child
      (if-let* ((child-heading (ms-forward-child obj)))
          (let* ((child (ms--make-slide
                         child-heading
                         (oref ms--deck slide)
                         :inline t
                         ;; TODO this won't compose at all
                         :slide-action #'ms-action-narrow
                         :slide-action-args '(:include-restriction t :with-children t)
                         :child-action 'none)))
            (let ((ms-slide-in-effect nil))
              (ms-end child))
            (push child (oref obj children)))
        (setq exhausted t)))))

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

(defun ms-hide-region (beg end &optional keep-fill)
  "Return overlay hiding region between BEG and END.
Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (let ((ov (make-overlay beg end)))
    (if keep-fill
        (let ((background (face-attribute 'default :background)))
          (overlay-put ov 'face `(:foreground ,background :background ,background)))
      (overlay-put ov 'display ""))
    ov))

(defun ms-hide-element (element &optional keep-fill)
  "Return an overlay that will hide ELEMENT.
Element is an org element.Optional KEEP-FILL will obscure but not
change the contents of text, keeping its height and width for
filling in other content."
  (ms-hide-region (org-element-property :begin element)
                  (org-element-property :end element)
                  keep-fill))

(defun ms-hide-item (item &optional keep-fill)
  "Return an overlay that hides ITEM.
See `org-item-struct' for structure of ITEM.  Note, this hides
the entire item, which may contain sub-items, but revealing
children of a hidden parent doesn't really make sense.

Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (ms-hide-region
   (car item) (car (last item)) keep-fill))

(defun ms-hide-contents (element &optional keep-fill)
  "Return an overlay that hides the contents of ELEMENT.
Element is an org element.  You should probably not use this on
headings because their contents includes the sections and the
children.  See `ms-hide-section' and `ms-hide-children'.

Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (ms-hide-region (org-element-property :contents-begin element)
                  (org-element-property :end element)
                  keep-fill))

(defun ms-hide-section (heading &optional keep-fill)
  "Return an overlay that hides the section of HEADING.
HEADING is a headline type org element.

Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (ms-hide-region
   (ms--section-begin heading)
   (ms--section-end heading)
   keep-fill))

(defun ms-hide-children (heading &optional keep-fill)
  "Return an overlay that hides the children of HEADING.
HEADING is a headline type org element.

Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (ms-hide-region
   (ms--section-end heading)
   (org-element-property :end heading)
   keep-fill))

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
        (narrow-to-region (org-element-property :begin element)
                          (org-element-property :end element))
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
        (when-let ((begin (org-element-property :contents-begin element))
                   (end (org-element-property :contents-end element)))
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
                         (lambda (e) (> (org-element-property :begin e) after)))))
    (ms--section-map
     heading type combined-pred info t no-recursion)))

(defun ms--section-previous
    (heading type before &optional pred info no-recursion)
  "Return previous element of TYPE that starts before BEFORE.
Optional PRED should accept ELEMENT and return non-nil if
matched."
  (let* ((combined-pred (ms-and
                         pred
                         (lambda (e) (< (org-element-property :begin e) before)))))
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
      (org-element-property :begin section)
    (or (org-element-property :contents-begin heading)
        (org-element-property :end heading))))

(defun ms--section-end (heading)
  "Always returns a point, even for empty headings."
  (let ((not-self (lambda (e) (unless (equal (org-element-property :begin e)
                                        (org-element-property :begin heading))
                           e))))
    (if-let ((section-or-heading (ms--map
                                  heading '(headline section)
                                  not-self nil t t)))
        (if (eq (org-element-type section-or-heading)
                'headline)
            (org-element-property :begin section-or-heading)
          (org-element-property :end section-or-heading))
      (or
       (org-element-property :contents-begin heading)
       (org-element-property :end heading)))))

;; TODO these two functions behaved badly and rely on non-element methods of
;; unknown behavior
(defun ms--previous-sibling (heading &optional predicate)
  "Return the previous sibling HEADING if it exists.
PREDICATE should accept an ELEMENT argument and return non-nil."
  (without-restriction
    (save-excursion
      (goto-char (org-element-property :begin heading))
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
      (goto-char (org-element-property :begin heading))
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
  (let ((parent (org-element-property :parent element)))
    (while parent
      (if (or (not type)
              (ms-type-p parent type))
          (setq element parent
                parent (org-element-property :parent parent))
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
Does not modify the point."
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

;; These variables were brought forward from org-tree-slide.  There's not
;; sufficient reason to upgrade them to customize variables nor remove.

;; TODO these can be used across buffers when set before cloning indirect
;; buffers, but that's a coincidence, not necessarily a design choice.
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
;; TODO allow header override function
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

;; TODO move respect for animation variables into this function
;; TODO END is a redundant argument unless a virtual newline is introduced.
;; Test if an overlay can can work via after-string.
;; TODO Support non-graphical
;; TODO Inline animation fallback, uncover text character by character.
;; TODO User-provided animation override function
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

(defun ms--debug (slide &optional situation)
  (when ms--debug
    (let* ((heading (ms-heading slide))
           (headline-begin (org-element-property :begin heading))
           (headline-end (or (org-element-property :contents-begin heading)
                             (org-element-property :end heading)))
           (situation (or situation
                          "ms--debug")))
      (message "%s begin: %s heading: %s"
               situation
               (marker-position (oref slide begin))
               (save-restriction
                 (widen)
                 (buffer-substring headline-begin (1- headline-end)))))))

(defun ms--cleanup-state ()
  "Clean up states between contents and slides."
  (ms--delete-header)
  (ms--delete-overlays)
  (ms--animation-cleanup)
  (mapc (lambda (f) (funcall f nil))
        (oref ms--deck step-callbacks))
  (oset ms--deck step-callbacks nil)
  (remove-hook 'post-command-hook #'ms--contents-hl-line t))

(defun ms--ensure-deck ()
  "Prepare for starting the minor mode.
Call this when writing commands that could be called before or
after a deck exists but should create a deck if it does not exist.

In functions that should only be called when a deck is alive and
associated with the current buffer, use `ms-live-p'
and throw an error if it's not live.

This function sets up the deck.  Many operations such as calling
hooks must occur in the deck's :slide-buffer."
  (unless (ms-live-p)
    ;; Prevent starting within indirect buffers
    (when (buffer-base-buffer (current-buffer))
      (error "Buffer is indirect but deck is already live"))

    ;; TODO check assumed initial conditions
    (let* ((base-buffer (current-buffer))
           (slide-buffer-name (format "*deck: %s*" (buffer-name
                                                    base-buffer))))

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
        (display-buffer slide-buffer ms--display-actions)
        (set-buffer slide-buffer)

        (widen)
        (org-fold-show-all)
        ;; Enter the state model
        (ms--choose-slide deck
                          ms-start-from)))))

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
  (when ms--contents-hl-line-overlay
    (delete-overlay ms--contents-hl-line-overlay))
  (setq ms--animation-overlay nil
        ms--animation-timer nil
        ms--contents-hl-line-overlay nil))

(defun ms--ensure-slide-buffer (&optional display)
  "Run in commands that must run in the slide buffer.
Unless optional DISPLAY is non-nil, the buffer is only set."
  (unless (ms-live-p)
    (error "Live deck not found"))
  (if display
      (display-buffer (oref ms--deck slide-buffer)
                      ms--display-actions)
    (set-buffer (oref ms--deck slide-buffer))))

(defun ms--keyword-value (key)
  "Get values like #+KEY from document keywords."
  (cadr (assoc-string key (org-collect-keywords `(,key)))))

(defun ms--feedback (key)
  "Explicit feedback for commands without visible side effects."
  (when-let ((feedback (plist-get ms-feedback-messages
                                  key)))
    (let ((message-log-max nil))
      (message "%s" feedback))))

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
              (push (intern-soft (pop tokens)) class-with-args)
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

;; * Contents Highlight Line

;; Basically we need to use the post-command hook to update a line with our
;; handy-dandy face.  This is basically just a less feature-ful re-implementation
;; of hl-line.  hl-line is kind of subtle and works across all buffers.  This is
;; safer and defaults to the face attribute, :inverse-video, which is super high
;; contrast, good when navigating slide headlines like a menu.
(defun ms--contents-hl-line ()
  (unless ms--contents-hl-line-overlay
    (setq ms--contents-hl-line-overlay
          (make-overlay (point) (point)))
    (overlay-put ms--contents-hl-line-overlay
                 'face 'ms-contents-selection-face)
    (overlay-put ms--contents-hl-line-overlay
                 'priority 10))
  (when-let ((element (org-element-at-point)))
    (setf (overlay-start ms--contents-hl-line-overlay)
          (1+ (org-element-property :begin element)))
    (setf (overlay-end ms--contents-hl-line-overlay)
          (org-element-property :end element))))

;; * Lifecycle

(defvar-keymap ms-mode-map
  :doc "The keymap for `ms' mode."
  "<left>" #'ms-backward
  "<right>" #'ms-forward
  "<up>" #'ms-start
  "<down>" #'ms-stop)

;;;###autoload
(define-minor-mode ms-mode
  "A presentation tool for Org Mode."
  :init-value nil
  :interactive nil
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
         (funcall (or ms-start-function #'ms-display-slides))
         (run-hooks 'ms-start-hook)
         (ms--feedback :start))
        (t
         (ms--stop))))

(defun ms-live-p ()
  "Check if a deck is associated so that commands can complete."
  (and ms-mode
       ms--deck
       (ms-deck-live-p ms--deck)))

;; TODO rename these functions to `switch-to'?
(defun ms-display-slides ()
  (ms--ensure-slide-buffer t)
  (ms--cleanup-state)
  (oset ms--deck slide-buffer-state 'slides)
  (widen)
  (org-fold-show-all)
  (ms-init ms--deck))

(defun ms--follow (progress)
  "Set the base buffer window point to PROGRESS.
PROGRESS must be an integer buffer location, not a marker."
  (unless (ms-live-p)
    (error "Live deck not found"))
  (let ((pos (cond ((integerp progress) progress)
                     ((eieio-object-p progress)
                      (marker-position (oref progress begin)))
                     ((markerp progress) (marker-position progress)))))
    (when (null pos)
      (warn "Progress was null! %s" progress))
    (when (and pos ms-base-follows-slide)
      (set-buffer (oref ms--deck base-buffer))
      (unless (and (>= pos (point-min))
                   (<= pos (point-max)))
        (widen))
      (goto-char pos)
      (pulse-momentary-highlight-one-line pos 'ms-highlight)
      ;; TODO maybe only two of these are actually necessary
      (org-fold-show-context)
      (org-fold-show-entry)
      (org-fold-show-subtree)
      (when-let ((windows (get-buffer-window-list (current-buffer))))
        (mapc (lambda (w) (set-window-point w pos)) windows))
      (set-buffer (oref ms--deck slide-buffer)))))

(defun ms-display-contents ()
  "Switch to showing contents in the slide buffer.
This is a valid `ms-start-function' and will start
each slide show from the contents view."
  (ms--ensure-slide-buffer t)
  (ms--cleanup-state)
  (oset ms--deck slide-buffer-state 'contents)

  (widen)
  (org-overview)
  (recenter)

  (when ms-contents-header
    (if-let ((first (ms--document-first-heading)))
        (narrow-to-region (org-element-property :begin first)
                          (point-max))
      ;; No first heading.  Just header.  Empty contents.
      (narrow-to-region (point-max)
                        (point-max)))
    (run-hooks 'ms-narrow-hook)
    (ms--make-header t))

  (when ms-contents-selection-highlight
    (add-hook 'post-command-hook #'ms--contents-hl-line nil t))

  ;; TODO walk all headings with the filter and add overlays on the hidden stuff
  ;; TODO filter slides that don't have a display action?

  (ms--feedback :contents)
  (run-hooks 'ms-contents-hook))

(defun ms--stop ()
  "Stop the presentation entirely.
Kills the indirect buffer, forgets the deck, and displays the
source buffer."
  (interactive)
  (when-let* ((deck ms--deck)
              (slide-buffer (oref deck slide-buffer))
              (base-buffer (oref deck base-buffer)))

    ;; TODO possibly finalize in state cleanup.  Slides <-> contents switching
    ;; may require attention.
    (with-demoted-errors "Deck finalization failed: %s"
      (ms-final ms--deck))

    ;; Animation timers especially should be stopped
    ;; TODO ensure cleanup is thorough even if there's a lot of failures.
    (ms--cleanup-state)

    (setq ms--deck nil)

    (display-buffer base-buffer ms--display-actions)
    (set-buffer base-buffer)

    (when slide-buffer
      (kill-buffer slide-buffer))

    (when ms-mode
      (ms-mode -1))

    (run-hooks 'ms-stop-hook)
    (ms--feedback :stop)))

;; * User Commands

;;;###autoload
(defun ms-stop ()
  "Stop the presentation.
It is recommended to not bind this to a controller button unless
you have five buttons or will use the display button to stop and
can reliably select displays via other means."
  (interactive)
  (ms--stop))

;;;###autoload
(defun ms-start ()
  "Start presentation or secondary action.
It is recommended to bind this in the `org-mode-map'.  It starts
the mode if the mode is inactive.

It is also recommended to bind this to the play button on a
presentation controller.  Its behavior will be overloaded with a
secondary action, such as playing a video on the slide, if one is
available.  The default secondary task is the contents view.

TODO Add support for arbitrary secondary tasks like playing a
video or custom actions."
  (interactive)
  (if (ms-live-p)
      (progn (ms--ensure-slide-buffer)
             (if (ms--showing-slides-p)
                 ;; TODO check for secondary task here
                 (ms-display-contents)
               (ms--choose-slide ms--deck 'contents)
               (ms-display-slides)))
    (let ((ms-start-function
           #'ms-display-slides))
      (ms-mode 1))))

;;;###autoload
(defun ms-forward ()
  "Advance slideshow forward."
  (interactive)
  (ms--ensure-slide-buffer)
  (if (ms--showing-contents-p)
      (org-next-visible-heading 1)
    (ms--ensure-slide-buffer)
    (ms-step-forward ms--deck)))

;;;###autoload
(defun ms-backward ()
  "Advance slideshow backward."
  (interactive)
  (ms--ensure-slide-buffer)
  (if (ms--showing-contents-p)
      (org-previous-visible-heading 1)
    (ms--ensure-slide-buffer)
    (ms-step-backward ms--deck)))

(provide 'macro-slides)
