;;; dslide.el --- Domain Specific sLIDEs. A presentation framework -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2011-2023 Takaaki ISHIKAWA
;; Copyright (C) 2024 Positron
;;
;; Author: Positron <contact@positron.solutions>
;; Version: 0.3.2
;; Package-Requires: ((emacs "29.2"))
;; Maintainer: Positron <contact@positron.solutions>
;; URL: https://github.com/positron-solutions/dslide
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

;; DSL IDE is a highly programmable presentation framework that first displays
;; Org files as presentations but also can integrate your presentation with any
;; Emacs buffer and also with Org Babel.  By integrating arbitrary Emacs Lisp
;; into the simple forward-backward user interface, you can make anything Emacs
;; does easy to present.
;;
;; See the README and manual M-x info-display-manual RET dslide RET.
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
;;    2. Run `dslide-deck-start'
;;    3. Use arrow keys.  See `dslide-mode-map'
;;
;; Note:
;;    - Customize variables, M-x customize-group RET dslide RET
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
;; For users, you might want to create your own actions, so check `dslide-action'
;; and its sub-classes.  Read the manual on `(info \"(eieio) \Top")'
;;
;; The `dslide-deck' class contains some functions related to adding callbacks or
;; entering custom sequences.
;;
;; For hackers wishing to extend the code, in addition, you will want to check
;; `dslide--make-slide' if you want your slides to hydrate actions differently.
;; Also pay very close attention to `dslide-stateful-sequence' and how sequences and
;; steps can be pushed.
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

(defgroup dslide nil
  "User variables for `dslide'."
  :group 'outlines)

(defcustom dslide-base-follows-slide t
  "Non-nil moves the base buffer point to the current slide.
This happens whether the buffer is visible or not."
  :type 'boolean
  :group 'dslide)

(defcustom dslide-start-from 'first
  "When starting, begin at `point' `first' slide.
Any other value is equivalent to `first'.

If the contents are shown first, the point will be on the
configured slide.

This only has effect when starting the mode or commands that
implicitly start the mode.

- `first': Always begin the slideshow from the very first slide.

- `point': the slideshow always begins at the slide under point.

If you just want to navigate slides with the point, you should
use the contents mode by calling `dslide-deck-start' in a
presentation that is already started."
  :type '(choice (const :tag "First slide" first)
                 (const :tag "Slide at point" point))
  :group 'dslide)

(defcustom dslide-start-function #'dslide-display-slides
  "When starting the mode, this is the default starting function.
It should usually call `dslide-display-slides' or
`dslide-display-contents'.  You can build commands that
use `let' binding to temporarily set this variable in order to
start with a specific starting function."
  :type 'function
  :group 'dslide)

(defcustom dslide-header t
  "The status of displaying the slide header."
  :type 'boolean
  :group 'dslide)

(defcustom dslide-header t
  "Display header in contents buffer.
When this is disabled, the keywords for title etc will remain
visible, albeit scrolled away because of how `org-overview'
works."
  :type 'boolean
  :group 'dslide)

(defcustom dslide-header-author t
  "Show the email in the header.
If there is a #+author: keyword, it will be used."
  :type 'boolean
  :group 'dslide)

(defcustom dslide-header-email t
  "Show the email in the header.
If there is a #+email: keyword, it will be used."
  :type 'boolean
  :group 'dslide)

(defcustom dslide-header-date t
  "Show the date in the header.
If there is a #+date: keyword, it will be used.
The current time will be used as a fallback."
  :type 'boolean
  :group 'dslide)

(defcustom dslide-margin-title-above 0.5
  "Margin between header title and the top of the window.
Can be a float or integer."
  :type 'number
  :group 'dslide)

(defcustom dslide-margin-title-below 0.5
  "Margin between title and other header info.
Can be a float or integer."
  :type 'number
  :group 'dslide)

(defcustom dslide-margin-content 1.5
  "Margin between the slide header and its content.
Can be a float or integer."
  :type 'number
  :group 'dslide)

(defcustom dslide-slide-in-effect t
  "Using a visual effect of slide-in for displaying trees."
  :type 'boolean
  :group 'dslide)

(defcustom dslide-slide-in-blank-lines 15
  "Line height of the slide-in effect."
  :type 'number
  :group 'dslide)

(defcustom dslide-feedback-messages
  '(:start "Start! ▶"
           :forward "Forward ➡"
           :backward "⬅ Backward"
           :contents "Contents ☰"
           :stop "Stop ■"
           :after-last-slide "No more slides!")
  "Feedback messages for slide controls.
Turn off by setting to nil.  Plist keys and where they are used:

- :start `dslide-deck-start'

- :forward `dslide-deck-forward'

- :backward `dslide-deck-backward'

- :contents `dslide-display-contents'

- :stop `dslide-deck-stop'

- :after-last-slide: see `after-last-slide-hook'"

  :type 'plist
  :group 'dslide)

(defcustom dslide-breadcrumb-face '(t :inherit org-level-8)
  "Face added to the list of faces for breadcrumbs.
This can be a face name symbol or an anonymous font spec.  It
will be added to the face list, meaning it the original face's
properties remain unless shadowed."
  :type 'face
  :group 'dslide)

(defface dslide-header-overlay-face '((t :inherit default))
  "Face for `dslide--header-overlay'."
  :group 'dslide)

(defcustom dslide-breadcrumb-separator " 🢒 "
  "Delimiter for breadcrumbs or nil to turn off breadcrumbs."
  :type '(choice (const :tag "Don't display breadcrumbs" nil)
                 (string :tag "Delimiter"))
  :group 'dslide)

(defcustom dslide-breadcrumbs-hide-todo-state t
  "If non-nil, hide TODO states in the breadcrumbs."
  :type 'boolean
  :group 'dslide)

(defcustom dslide-animation-duration 1.0
  "How long slide in takes."
  :type 'number
  :group 'dslide)

(defcustom dslide-animation-frame-duration (/ 1.0 60.0)
  "Length between updates.
Increase if your so-called machine has trouble drawing."
  :type 'number
  :group 'dslide)

(defcustom dslide-start-hook nil
  "Runs after the slide buffer is created but before first slide.
Buffer is widened and fully visible."
  :group 'dslide
  :type 'hook)

(defcustom dslide-stop-hook nil
  "Runs in the base buffer after stopping."
  :group 'dslide
  :type 'hook)

(defcustom dslide-narrow-hook nil
  "Runs whenever the slide buffer restriction is updated.
Use this hook for behaviors that affect the displayed region.
Slides and sequences that do not display themselves or only
affect display in another buffer will not trigger this hook."
  :group 'dslide
  :type 'hook)

(defcustom dslide-contents-hook nil
  "Runs last after switching to contents."
  :group 'dslide
  :type 'hook)

(defcustom dslide-after-last-slide-hook '()
  "Run when forward is called but there is no next slide.
This can either provide feedback or quit immediately etc.
Consider using `dslide-push-step' and writing a callback that
only reacts to the `forward' state.  This callback will then only
run if the user immediately calls `dslide-deck-forward'
again.  `dslide-deck-stop' is another good choice."
  :group 'dslide
  :type 'hook)

(defcustom dslide-default-slide-action #'dslide-action-narrow
  "Action class with lifecycle around the section actions.
When stepping forward or backward, it is called before any
section action.  It's normal purpose is to update the buffer
restriction before section-actions are run.

You can configure this per-heading by setting the
SLIDE_ACTION keyword.  You can configure it for
the document default by adding an SLIDE_ACTION
keyword."
  :type 'function
  :group 'dslide)

;; TODO test the use of plist args
(defcustom dslide-default-section-actions '()
  "Actions that run within the section display action lifecycle.
It's value is a list of symbol `dslide-action' sub-classes or (CLASS . ARGS)
forms where ARGS is a plist.  Each subclass will be instantiated
into an action object.  See the symbol `dslide-action' class and its methods
to learn about writing custom actions.

Many section actions are no-op whenever the content doesn't
contain any elements they act on.  You can add classes to this
list in order to have default behaviors for some org elements.

You can configure this per-heading by setting the
DSLIDE_SECTION_ACTIONS keyword.  You can configure it for the
document default by adding an DSLIDE_SECTION_ACTIONS keyword."
  :type '(list function)
  :group 'dslide)

(defcustom dslide-default-child-action #'dslide-child-action-slide
  "Action run after section lifecycle.
Value is an action class, usually extending
symbol `dslide-child-action'.  The usual purpose is to manage
the child headings, which come after the section element.

You can configure this per-heading by setting the
DSLIDE_CHILD_ACTION keyword.  You can configure it for the
document default by adding an DSLIDE_CHILD_ACTION keyword."
  :type 'function
  :group 'dslide)

(defcustom dslide-default-class 'dslide-slide
  "A class to more deeply modify slide behavior.
Value should be a custom class extending `dslide'.  You can
override methods if the built-in implementation is insufficient.
Consider upstreaming changes.

You can configure this per heading by setting the DSLIDE_CLASS
property.  You can configure it for the document default by
adding an DSLIDE_CLASS keyword."
  :type 'symbol
  :group 'dslide)

(defcustom dslide-default-deck-class 'dslide-deck
  "A class to more deeply modify overall deck behavior.
Value should be a custom class extending symbol `dslide-deck'.
Use this to modify the root-level behaviors, including switching
to children and finding siblings.  You can configure this for the
document by adding the DSLIDE_ROOT_CLASS keyword."
  :type 'symbol
  :group 'dslide)

(defcustom dslide-default-filter #'dslide-built-in-filter
  "A function used to call next on children.
The function used as actions should accept an org element, a
`headline' type element and return it if it is a valid heading or
return nil if it should be skipped.

You can configure this per heading by setting the DSLIDE_FILTER
keyword.  You can configure it for the document default by adding
an DSLIDE_FILTER keyword."
  :type 'function
  :group 'dslide)

(defcustom dslide-contents-selection-highlight t
  "Show a highlight on the selected headline.
This is useful if you have some subtle cursor feature enabled for
your presentation and wouldn't otherwise know what line you are
on in the contents view.  The default is also just a way more
obvious display style."
  :type 'boolean
  :group 'dslide)

(defface dslide-contents-selection-face
  '((t :inherit org-level-1 :inverse-video t :extend t))
  "Face for highlighting the current slide root."
  :group 'dslide)

(defface dslide-highlight
  '((t :inherit hl-line))
  "Face used in base buffer to highlight progress.
See `dslide-base-follows-slide'."
  :group 'dslide)

(defface dslide-babel-success-highlight
  '((t :inherit hl-line))
  "Temporarily highlight babel blocks that succeeded."
  :group 'dslide)

(defface dslide-babel-error-highlight
  '((t :inherit error))
  "Temporarily highlight babel blocks that failed."
  :group 'dslide)

(defvar dslide--debug nil
  "Set to t for logging slides and actions.")

(defvar dslide--animation-timers nil)
(defvar-local dslide--animation-overlays nil)

;; Tell the compiler that these variables exist
(defvar dslide-mode)

(defvar dslide--deck nil
  "Active deck object.
This is global.  If a presentation is active, you can look at this variable to
coordinate with it.")

(defvar dslide--overlays nil
  "Overlays used to hide or change contents display.")

(defvar dslide--step-overlays nil
  "Overlays that only live for one step.")

(defvar dslide--header-overlay nil
  "Flag to check the status of overlay for a slide header.")

;; Shouldn't need one per buffer
(defvar dslide--contents-hl-line-overlay nil
  "Highlights selected heading in contents view.")

(defconst dslide--display-actions
  '(display-buffer-same-window display-buffer-in-previous-window)
  "Configure `display-buffer-alist' to override.")

;; * Classes
;; - `dslide-deck': is the first thing called into by
;;   `dslide-deck-forward' and `dslide-deck-backward'.
;;
;; - `dslide-slide': interprets an org heading into some actions, which
;;
;; - `dslide-action': does most of the actual work of narrowing, hiding,
;;   animating, executing babel etc.

;; The generic functions below are the most important interfaces for all hacking
;; of this package.
;;
;; The domain model first must describe a linear sequence of steps that the user
;; traverses both forward and backward.
;;
;; There are some states that may need to be set up or torn down at the
;; boundaries of the sequence.  These are handled by three methods:
;; - `dslide-begin'
;; - `dslide-end'
;; - `dslide-final'
;;
;; `dslide-end' is essentially begin for going in reverse.  Usually this is the
;; same as calling begin and then stepping forward until no more progress is
;; made.  However doing it this way would be unable to avoid extra work and
;; could even create headaches when implementing sequences that shouldn't use
;; reverse to un-execute the forwards steps or in cases where implementing this
;; is too complex to pay off to the user.  For these reasons, the implementation
;; of `dslide-end' is left up to the user.
;;
;; `dslide-goto' essentially is just a careful use of forward.  If every forward
;; step properly reports its maximum extent of progress, we can use forward and
;; begin to implement every goto.
;;
;; Finally, `dslide-forward' and `dslide-backward' should navigate the states
;; between begin or end and final.  They just return non-nil until they are
;; done.  The caller doesn't care about the implementation, and that is why
;; EIEIO is used.
;;
;; Sub-sequences can rely on the parent state to exist for their entire
;; lifetime. The parent sequence will not call its own `dslide-final' until
;; after it has called the sub-sequence's `dslide-final'.
;;
;; Sub-sequences currently don't have any first-class extensible support for
;; entering or exiting the sub-sequence.  Such cooperation is present in limited
;; amounts to limit coupling the parent and child sequences.
;;
;; A lazy implementer can forego methods by delegating them to simpler
;; idempotent methods, such as using an idempotent begin for backward.  With
;; a maximum of six methods and a minimum of two, just begin and forward, you
;; have enough behavior to properly fit the user interface.

(cl-defgeneric dslide-begin (obj)
  "Set up the initial state of OBJ when going forward.
The sequence is being entered from its beginning.

Return values are ignored.  `dslide-begin' always counts as a
step because it's a result of a nil return from
`dslide-deck-forward'.

This method should work together with `dslide-end' and
`dslide-final' to ensure consistently valid state for
`dslide-deck-forward' and `dslide-deck-backward'.")

(cl-defgeneric dslide-end (obj)
  "Set up the initial state of OBJ when going backward.
The sequence is being entered from the end.

Return values are ignored.  `dslide-end' always counts as a step
because it's a result of a nil return from
`dslide-deck-backward'.

The first job of this method is to perform setup, possibly by
just calling begin since they likely have similar side-effects.

Second, this method should reach the state that is equivalent to
if the user called forward until no more progress could be made.

The default implementation calls `dslide-begin' and then calls
`dslide-forward' until no more progress can be made.  If this is
inappropriate, it should be overridden.

In cases where you don't need a real backward implementation or
progressing backwards would have no sensible behavior, you can
delegate this to `dslide-begin' and possibly delegate
`dslide-deck-backward' to `dslide-deck-forward',
resulting in a sequence that always starts at the beginning and
always proceeds to the end.  For a single step sequence that has
identical effect in both directions, this is appropriate.

This method should work together with `dslide-end' and
`dslide-final' to ensure consistently valid state for
`dslide-deck-forward' and `dslide-deck-backward'")

(cl-defgeneric dslide-final (obj)
  "Clean up any remaining state of OBJ.
Implement this method to clean up any state that would interfere
with the sequence succeeding when run again.  If your sequence
implements real backward behavior,

All side-effects and states created by steps in the sequence or
the `dslide-begin' and `dslide-end' methods must be cleaned up or
otherwise managed or else `dslide-backward' and other sequences
of running a presentation will be brittle and likely fail when
re-run.")

(cl-defgeneric dslide-forward (obj)
  "Advance OBJ forward by one step.
The return value has meaning to the deck:

- t: progress was made

- a point: progress was made up to a specific buffer location

- nil: no progress could be made.

For sequences that don't make progress in a buffer, returning t
is fine.  Returning a point of progress is necessary for the
default implementation of `dslide-goto'.

⚠ Every sequence repeated calls to of `dslide-forward' should
return nil at some point or else infinite loops will result.")

(cl-defgeneric dslide-backward (obj)
  "Advance OBJ backward by one step.
The return value has meaning to the deck:

- t: progress was made

- a point: progress was made up to a specific buffer location

- nil: no progress could be made.

For sequences that don't make progress in a buffer, returning t
is fine.  Returning a point of progress is necessary for the
default implementation of `dslide-goto'.

⚠ Every sequence of repeated calls to `dslide-backward' should
return nil at some point or else infinite loops will result.")

(cl-defgeneric dslide-goto (obj point)
  "Advance OBJ forward beyond POINT.
This method can usually be implemented on top of
`dslide-forward' by advancing until POINT is exceeded.  Return
nil if POINT was not exceeded.  Return non-nil if the sense of
progress exceeds POINT.  Usually, child actions will be
responsible for determining if the POINT belongs to this slide or
one of its child slides, and the slide will just ask the child
action.")

;; ** Stateful Sequence
(defclass dslide-stateful-sequence ()
  ;; TODO parent slot is possibly vestigial
  ((parent
    :initval nil
    :initarg :parent
    :documentation "Parent or root sequence."))

  "An interface definition for linear sequences of steps.
This is an abstract class.

The sequence can be traversed forwards and backward.  `begin' and
`foward' are conjugates of `end' and 'backward'.

Because the sequence steps may rely on some setup and should
perform necessary teardown, the stateful sequence provides `begin'
`end' and `final' methods.

It can also be indexed by high-level navigation commands.  The
implementation of `dslide-goto' Sequences can run as sub-sequences,
where one sequence calls into another.  🚧 This capability is largely
unimplemented, but compatible with existing work.

Classes that wish to implement the stateful sequence interface
just need to support a few methods and then rely on the generic
implementations for the rest, unless they want to optimize or
simplify their implementation."
  :abstract t)

(cl-defmethod dslide-begin ((_ dslide-stateful-sequence)))

(cl-defmethod dslide-end ((obj dslide-stateful-sequence))
  (let ((progress t))
    (while progress
      (setq progress (dslide-forward obj)))))

(cl-defmethod dslide-forward ((_ dslide-stateful-sequence)))

(cl-defmethod dslide-backward ((_ dslide-stateful-sequence)))

(cl-defmethod dslide-final ((_ dslide-stateful-sequence)))

(cl-defmethod dslide-goto ((obj dslide-stateful-sequence) point)
  (unless (eq 'skip (dslide-begin obj))
    (let (exceeded (advanced t))
      (while (and advanced (not exceeded))
        (let ((progress (dslide-forward obj)))
          (if (and (numberp progress)
                   (>= progress point))
              (setq exceeded t)
            (setq advanced progress)))))))

;; ** Parent
;; TODO this class is kind of half-baked.  It was intended to wrap up the
;; filtering functionality and needing to find next and previous children.
;; Needs actual usage to become mature.
(defclass dslide-parent ()
  ((filter
    :initform nil
    :initarg :filter
    :documentation "Function to filter child headings."))
  "Class for objects that contain children.")

;; TODO highly indirect and delegates down to a really crappy implementation
;; that nobody else should ever want to use
(cl-defmethod dslide-next-child ((obj dslide-parent) child)
  "Get the next unfiltered CHILD of OBJ."
  (dslide-next-sibling
   child (oref obj filter)))

(cl-defmethod dslide-previous-child ((obj dslide-parent) child)
  "Get the previous unfiltered CHILD of OBJ."
  (dslide-previous-sibling
   child (oref obj filter)))

;; ** Deck
;; TODO extract non-org-specific behavior to sequence-root class.
(defclass dslide-deck (dslide-parent)
  ((slide
    :initform nil
    :documentation "The active sequence or slide.
This is probably a `dslide-slide' object, but anything
that implements `dslide-stateful-sequence' will probably
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

  "Root sequence that dispatches commands to slides.
Holds states such as those needed when switching between slides
and contents.  Is responsible for picking root headings and hydrating them into
slides and their actions.

Class can be overridden to affect root behaviors.  See
`dslide-default-deck-class'")

(cl-defmethod dslide-begin ((obj dslide-deck))
  "Initialize the first slide of OBJ."
  (unless (oref obj slide)
    ;; Calls implied from other commands should have started the lifecycle already
    (error "No slide selected"))

  ;; TODO This line is critical to starting up the state machine.  Slides
  ;; are still inferring their need to narrow.
  (narrow-to-region (point) (point))    ; signal to slide to draw itself
  (dslide-begin (oref obj slide)))

(cl-defmethod dslide-end ((_ dslide-deck))
  (error "Deck has no valid concept of starting at the end"))

(cl-defmethod dslide-final ((obj dslide-deck))
  (when-let ((slide (oref obj slide)))
    (dslide-final slide)))

;; Deck forward & backward methods are the entry point for user forward and
;; backward commands.  They delegate out to slides, which may telescope into
;; their children in order to make progress.
;;
;; It make require several trips through the behavior to consume callbacks that
;; are run for effect or are no-op, things that don't count as steps or are
;; slides that decide at runtime to be skipped.
;;
;; In short, loop through whatever next steps and callbacks were pushed onto the
;; stack.  When one of them makes progress, we're done.

(cl-defmethod dslide-forward ((obj dslide-deck))
  (unless (oref obj slide)
    ;; Calls implied from other commands should have started the lifecycle
    ;; already
    (error "No slide selected"))

  (while dslide--step-overlays
    (delete-overlay (pop dslide--step-overlays)))

  (let (progress reached-end)
    ;; Burn up a step callback until one returns non-nil
    (when-let ((steps (oref obj step-callbacks)))
      (while (and (not progress)
                  steps)
        (setq progress (funcall (pop steps) 'forward)))
      (oset obj step-callbacks steps))

    (while (not (or progress reached-end))
      (let* ((current-slide (oref obj slide))
             (result (dslide-forward current-slide))
             next-slide)

        (if result
            (setq progress result)
          ;; Check if there is a next sibling.
          (if-let ((next-child (dslide-next-child obj current-slide)))
              (setq next-slide next-child)
            (setq reached-end t)))

        (unless next-slide
          (dslide--debug current-slide (format "forward: %s" progress)))

        (when next-slide
          (dslide--debug next-slide "switching to sibling")
          (oset obj slide next-slide)
          (dslide-final current-slide)

          (dslide-begin next-slide)
          ;; Begin counts as a step
          (setq progress next-slide))))

    ;; A lot of progress may have happened, but there will be only one feedback
    ;; message.
    (when progress
      (dslide--feedback :forward)
      (dslide--follow progress))

    (when reached-end
      (dslide--feedback :after-last-slide)
      (run-hooks 'dslide-after-last-slide-hook))))

(cl-defmethod dslide-backward ((obj dslide-deck))
  (unless (oref obj slide)
    ;; Calls implied from other commands should have started the lifecycle
    ;; already
    (error "No slide selected"))

  (while dslide--step-overlays
    (delete-overlay (pop dslide--step-overlays)))

  ;; Going backward is almost the same as going forward.  The big difference is
  ;; that when a slide is instantiated, it needs to be sent to its end.  Usually
  ;; the default implementation, which calls forward until progress is
  ;; exhausted, is fine.  Certain actions with side-effects may not like this,
  ;; and they should implement an actual `dslide-end' method as well as idempotent
  ;; `dslide-begin' and `dslide-final' if any support for going backwards is desirable.

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
             (result (dslide-backward current-slide))
             previous-slide)

        (if result
            (setq progress result)
          ;; Check if there is a previous sibling.
          (if-let ((previous-child (dslide-previous-child
                                    obj current-slide)))
              (setq previous-slide previous-child)
            (setq reached-beginning t)))

        (unless previous-slide
          (dslide--debug current-slide (format "forward: %s" progress)))

        (when previous-slide
          (dslide--debug previous-slide "switching to sibling")
          (oset obj slide previous-slide)
          (dslide-final current-slide)

          ;; end counts as a step.
          (dslide-end previous-slide)
          (setq progress previous-slide))))

    ;; A lot of progress may have happened, but there will be only one feedback
    ;; message.
    (cond (progress
           (dslide--feedback :backward)
           (dslide--follow progress))
          (reached-beginning
           (user-error "No more previous slides!")))))

(cl-defmethod dslide--choose-slide ((obj dslide-deck) how)
  "Set the current slide of OBJ, according to HOW."
  ;; TODO apply filter when choosing starting slide
  (cond ((eq how 'first)
         (oset obj slide (dslide--make-slide
                          (dslide--document-first-heading) obj)))
        ((eq how 'contents)
         (oset obj slide (dslide--make-slide
                          (dslide--root-heading-at-point (point))
                          obj)))
        ((eq how 'point)
         (let ((base-point (with-current-buffer (oref obj base-buffer)
                             (point))))
           ;; TODO implement looking inside the slides using `goto' and recover
           ;; the child with a point argument.
           (oset obj slide
                 (dslide--make-slide
                  (dslide--root-heading-at-point base-point) obj))))))

(cl-defmethod dslide-deck-live-p ((obj dslide-deck))
  "Check if all of OBJ's buffers are alive or can be recovered."
  ;; TODO in some circumstances, an indirect buffer might exist, but we should
  ;; probably kill it if it was created outside the current instance's lifecycle
  (and (buffer-live-p (oref obj base-buffer))
       (buffer-live-p (oref obj slide-buffer))
       (eq (oref obj base-buffer) (buffer-base-buffer
                                   (oref obj slide-buffer)))))

(defun dslide-push-window-config (&optional step)
  "Save the window configuration and narrowing for restoration.

Optional STEP argument will decide if the callback counts as a step or will
return nil so that it is only run for effects."
  (let ((window-config (current-window-configuration)))
    (dslide-push-step
     (lambda (_) (prog1 step
              (set-window-configuration window-config))))))

(defun dslide-push-step (fun)
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
  (unless (dslide-live-p)
    (error "No active deck"))
  (push fun (oref dslide--deck step-callbacks)))

;; * Slide
(defclass dslide-slide (dslide-parent dslide-stateful-sequence)
  ((slide-action
    :initform nil :initarg :slide-action
    :documentation "Action run around both section and child actions.
See `dslide-default-slide-action'.")
   (section-actions
    :initform nil :initarg :section-actions
    :documentation "Typical actions that work on the section.
Live within slide action lifecycle.  See
`dslide-default-section-actions'.")
   (child-action
    :initform nil :initarg :child-action
    :documentation "Action run after section.
Live within slide action lifecycle. See
`dslide-default-child-action'.")
   (begin
    :initform nil :initarg :begin
    :documentation "Marker for retrieving this heading's org element."))

  "Stores and coordinates the actions of a heading.
The life-cycles of actions that run for a heading overlap, and
the slide object coordinates this overlap.  It delegates the
`stateful-sequence' calls into the actions in the appropriate
order.")

(cl-defmethod dslide-begin ((obj dslide-slide))
  (when-let ((slide-action (oref obj slide-action)))
    (dslide-begin slide-action))
  (when-let ((section-actions (oref obj section-actions)))
    (mapc #'dslide-begin section-actions))
  (when-let ((child-action (oref obj child-action)))
    (dslide-begin child-action)))

(cl-defmethod dslide-end ((obj dslide-slide))
  (when-let ((slide-action (oref obj slide-action)))
    (dslide-end slide-action))
  ;; Fairly certain the ordering of child and section actions doesn't actually
  ;; matter for `dslide-end', but this ordering matches the situation that would
  ;; occur if the user just called `dslide-forward' repeatedly, and we want the
  ;; end state to be as close to "normal" as possible.
  (when-let ((section-actions (oref obj section-actions)))
    (mapc #'dslide-end (reverse section-actions)))
  (when-let ((child-action (oref obj child-action)))
    (dslide-end child-action)))

(cl-defmethod dslide-final ((obj dslide-slide))
  ;; The order that these are called shouldn't matter.  No use case for coupling
  ;; different finals, but the guarantee is that the lifecycle of the slide
  ;; actions encompass the contents actions (child and section)
  (mapc (lambda (action)
          (dslide-final action))
        (oref obj section-actions))
  (when-let ((child-action (oref obj child-action)))
    (dslide-final child-action))
  (when-let ((display-action (oref obj slide-action)))
    (dslide-final display-action))
  ;; Clean up heading marker, which is shared by children
  (set-marker (oref obj begin) nil))

(cl-defmethod dslide-forward ((obj dslide-slide))
  (let ((section-actions (oref obj section-actions))
        (child-action (oref obj child-action))
        (slide-action (oref obj slide-action))
        progress)
    (while (and (not progress) section-actions)
      (setq progress (dslide-forward (pop section-actions))))
    (unless (or progress (null child-action))
      (setq progress (dslide-forward child-action)))
    (unless (or progress (null slide-action))
      (setq progress (dslide-forward slide-action)))
    progress))

(cl-defmethod dslide-backward ((obj dslide-slide))
  (let ((section-actions (oref obj section-actions))
        (child-action (oref obj child-action))
        (slide-action (oref obj slide-action))
        progress)
    (unless (null child-action)
      (setq progress (dslide-backward child-action)))
    (while (and (not progress) section-actions)
      (setq progress (dslide-backward (pop section-actions))))
    (unless (or progress (null slide-action))
      (setq progress (dslide-backward slide-action)))
    progress))

;; `dslide--make-slide' is very critical to the user-facing configuration and
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
;; `dslide--make-slide' supports plist arguments when hydrating from org properties
;; and why child actions that create slides can pass these in via `args'.

(defun dslide--make-slide (heading parent &rest args)
  "Hydrate a slide object from a HEADING element.
Many optional ARGS.  See code.  PARENT is possibly redundant and
may be refactored out."
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
                      '("DSLIDE_SLIDE_ACTION"
                        "DSLIDE_SECTION_ACTIONS"
                        "DSLIDE_CHILD_ACTION"
                        "DSLIDE_FILTER"
                        "DSLIDE_CLASS")))

           (args `(:inline ,inline))

           ;; TODO just munged this a bit for explicit nil handling.  Might
           ;; still have precedence wrong.  If there is any string set in any
           ;; property, the default value shouldn't be used.
           (slide-action-class
            (or slide-action-class
                (if-let ((declared
                          (or (org-element-property :DSLIDE_SLIDE_ACTION heading)
                              (cdr (assoc-string "DSLIDE_SLIDE_ACTION"
                                                 keywords)))))
                    (dslide--parse-class-with-args declared)
                  dslide-default-slide-action)))

           ;; TODO precedences are out of wack.  Heading property should win
           ;; versus child heading, document, or default
           (slide-action (when slide-action-class
                           (if (consp slide-action-class)
                               (apply (car slide-action-class)
                                      :begin begin
                                      :marker (copy-marker begin)
                                      (append args
                                              slide-action-args
                                              (cdr slide-action-class)))
                             (apply slide-action-class
                                    :begin begin
                                    :marker (copy-marker begin)
                                    (append args
                                            slide-action-args)))))

           ;; TODO action arguments might make sense, such as telling nested
           ;; elements not to animate.  It's really hard for them to infer being
           ;; in an inline child versus an independent slide, even by looking at
           ;; the restriction.
           (section-action-classes
            (or (dslide--parse-classes-with-args
                 (or (org-element-property :DSLIDE_SECTION_ACTIONS heading)
                     (cdr (assoc-string "DSLIDE_SECTION_ACTIONS" keywords))))
                dslide-default-section-actions))
           (section-actions
            (mapcar
             (lambda (c) (when c
                      (if (consp c)
                          (apply (car c)
                                 :begin begin
                                 :marker (copy-marker begin)
                                 (append args (cdr c)))
                        (apply c
                               :begin begin
                               :marker (copy-marker begin)
                               args))))
             section-action-classes))

           ;; TODO Likely some precedence funk here.  Copied from above.
           (child-action-class
            (or child-action-class
                (if-let ((declared
                          (or (org-element-property :DSLIDE_CHILD_ACTION heading)
                              (cdr (assoc-string "DSLIDE_CHILD_ACTION"
                                                 keywords)))))
                    (dslide--parse-class-with-args declared)
                  dslide-default-child-action)))

           (child-action (when (and child-action-class
                                    (not (eq child-action-class 'none)))
                           (if (consp child-action-class)
                               (apply (car child-action-class)
                                      :begin begin
                                      :marker (copy-marker begin)
                                      (append args
                                              child-action-args
                                              (cdr child-action-class)))
                             (apply child-action-class
                                    :begin begin
                                    :marker (copy-marker begin)
                                    (append
                                     args
                                     child-action-args)))))
           (filter
            (or (dslide--filter
                 (or (org-element-property :DSLIDE_FILTER heading)
                     (cdr (assoc-string "DSLIDE_FILTER" keywords))))
                dslide-default-filter))
           (class
            (or (dslide--parse-class-with-args
                 (or (org-element-property :DSLIDE_CLASS heading)
                     (cdr (assoc-string "DSLIDE_CLASS"
                                        keywords))))
                dslide-default-class)))

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

(cl-defmethod dslide-next-sibling ((obj dslide-slide) filter)
  (when-let* ((heading (dslide-heading obj))
              (next-heading (dslide--next-sibling heading filter)))
    (dslide--make-slide next-heading (oref obj parent))))

(cl-defmethod dslide-previous-sibling ((obj dslide-slide) filter)
  (when-let* ((heading (dslide-heading obj))
              (previous-heading (dslide--previous-sibling heading filter)))
    (dslide--make-slide previous-heading (oref obj parent))))

(cl-defmethod dslide-heading ((obj dslide-slide))
  "Return the OBJ's heading element."
  (org-element-at-point (oref obj begin)))

;; * Actions
;;; Pre-built Actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Actions are stateful sequences.  They live on a slide.  They usually work on
;; either the section or the children, but there is no requirement that they are
;; exclusive to either.  Child actions should compose with section actions, such
;; as round-robin children cycling through each child's action's forward and
;; backward methods. TODO TODO TODO 🚧

;; ** Base Action
(defclass dslide-action (dslide-stateful-sequence)
  ((begin
    :initform nil
    :initarg :begin
    :documentation "Marker for beginning of heading.  Used to
re-hydrate the org element for use in mapping over the section etc.")
   (marker
    :initform nil
    :initarg :marker
    :documentation "Marker used to track progress.
It is initialized to the same value as the `begin' slot.")
   (inline
     :initform nil
     :initarg :inline
     :docuemntation "Draw as if surrounded by other contents.
This option allows actions that perform some animation to degrade
to some technique that works with contents above and below."))
  "Base class for most slide actions that work on a heading's contents."
  :abstract t)

(cl-defmethod dslide-heading ((obj dslide-action))
  "Return the OBJ's heading element."
  (let ((heading (org-element-at-point (oref obj begin))))
    (if (eq (org-element-type heading) 'headline)
        heading
      (error "Begin marker no longer points at a heading"))))

(cl-defmethod dslide-marker ((obj dslide-action) &optional pom)
  "Set OBJ's marker slot to POM or return marker position.
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

(cl-defmethod dslide-section-next ((obj dslide-action) type
                                   &optional pred info no-recursion)
  "Advance OBJ's marker by one element of TYPE and return element.
Optional PRED INFO and NO-RECURSION are the same as for
`dslide-contents-map'.

Marker is moved to the end of the heading if no matching element
is found.  This allows actions to differentiate being at the last
element from having been called one more time after being done
with the last element."
  (if-let ((next (dslide--section-next
                  (dslide-heading obj) type (dslide-marker obj)
                  pred info no-recursion)))
      (prog1 next
        (dslide-marker obj (org-element-property :begin next)))
    (dslide-marker obj (org-element-property :end (dslide-heading obj)))
    nil))

(cl-defmethod dslide-section-previous
  ((obj dslide-action) type &optional pred info no-recursion)
  "Move OBJ's marker backward by element of TYPE and return element.
Optional PRED INFO and NO-RECURSION are the same as for
`dslide-contents-map'.

Marker is moved to the beginning of the heading if no matching
element is found.  This allows actions to differentiate the begin
state from being at the first matching element."
  (if-let ((previous (dslide--section-previous
                      (dslide-heading obj) type (dslide-marker obj)
                      pred info no-recursion)))
      (prog1 previous
        (dslide-marker obj (org-element-property :begin previous)))
    (dslide-marker obj (org-element-property :begin (dslide-heading obj)))
    nil))

(cl-defmethod dslide-section-map ((obj dslide-action) type fun
                                  &optional info first-match no-recursion)
  "Map FUN over TYPE elements in OBJ's heading's section.
Optional PRED INFO FIRST-MATCH and NO-RECURSION are the same as
for `dslide-contents-map'."
  (dslide--section-map
   (dslide-heading obj)
   type fun info first-match no-recursion))

;; begin and end are using the defaults.  override these if inappropriate.

(cl-defmethod dslide-final ((obj dslide-action))
  (when-let ((marker (oref obj marker)))
    (set-marker marker nil)))

;; ** Default Slide Action
(defclass dslide-action-narrow (dslide-action)
  ((include-restriction
    :initform nil
    :initarg :include-restriction
    :documentation "Include the existing restriction.")
   (breadcrumbs
    :initform t
    :initarg :breadcrumbs
    :documentation "Show breadcrumbs in the header.")
   (header
    :initform t
    :initarg :header
    :documentation "Show header.")
   (with-children
    :initform nil :initarg :with-children
    :documentation "Narrow should include children.
The default, nil, narrows to the section only."))
  "Default slide action.

Most actions need the current slide to be narrowed to.  This
action is capable of performing such narrowing and informing the
deck of progress was made.")

(cl-defmethod dslide-narrow ((obj dslide-action-narrow))
  "Narrow to OBJ's heading.
This function must return nil when it performs no update to the
restriction, meaning no progress was made."
  (let* ((progress)
         (heading (dslide-heading obj))
         (begin (oref obj begin))
         (end (if (oref obj with-children)
                  (org-element-property :end heading)
                (dslide--section-end heading))))

    (if (oref obj include-restriction)
        (unless (and (<= (point-min) begin)
                     (>= (point-max) end))
          (narrow-to-region (min (point-min) begin)
                            (max (point-max) end))
          (run-hooks 'dslide-narrow-hook)
          (when dslide-slide-in-effect
            (dslide-animation-setup begin end))
          (setq progress begin))
      (unless (and (<= (point-min) begin)
                   (>= (point-max) end))
        ;; TODO overlay-based display
        (narrow-to-region begin end)
        (run-hooks 'dslide-narrow-hook)
        (let ((dslide-header (oref obj header)))
          (dslide--make-header (null (oref obj breadcrumbs))))
        (goto-char (point-min))         ; necessary to reset the scroll
        (when (and dslide-slide-in-effect
                   (not (oref obj inline)))
          (dslide-animation-setup begin end))
        (setq progress begin)))
    ;; Return progress to count as step when re-narrowing after a child.
    progress))

;; This code makes little sense.  See the slide's current ordering of calling
;; the slide action, and the reason will make sense.  A re-write will probably
;; get it right.  The key thing to note is that a parent can't re-display itself
;; unless it's going backwards.  It needs to display itself during end even
;; though the end of its children may clobber it.  This works, just awkwardly.
(cl-defmethod dslide-begin :after ((obj dslide-action-narrow))
  (dslide-narrow obj))

(cl-defmethod dslide-forward ((_ dslide-action-narrow)) ; odd
  nil)

(cl-defmethod dslide-backward ((obj dslide-action-narrow))
  (dslide-narrow obj))

(cl-defmethod dslide-end :after ((obj dslide-action-narrow))
  (dslide-narrow obj))

;; ** Reveal items section action
(defclass dslide-action-item-reveal (dslide-action)
  ((overlays
    :initform nil))
  "Hide all items and then reveal them one by one.")

(cl-defmethod dslide-begin :after ((obj dslide-action-item-reveal))
  (oset obj overlays
        (dslide-section-map
         obj 'item (lambda (e) (dslide-hide-element e (oref obj inline))))))

;; The default `dslide-end' method is sufficient since this action will
;; just add overlays starting from the end of items.

(cl-defmethod dslide-final :after ((obj dslide-action-item-reveal))
  (when-let ((overlays (oref obj overlays)))
    (mapc #'delete-overlay overlays)))

;; TODO add hide / un-hide methods to the base action
(cl-defmethod dslide-forward ((obj dslide-action-item-reveal))
  ;; The implementation has mapped all of the items into overlays, so instead of
  ;; calling `dslide-section-next', we just use the overlay positions to walk
  ;; through the items.
  (when-let* ((overlays (oref obj overlays))
              (first (car overlays))
              (end (overlay-end first))
              (start (overlay-start first)))
    ;; TODO We can let-bind animations false for child slides.
    ;; Or handle this via arguments in child actions
    (when dslide-slide-in-effect
      (dslide-animation-setup
       (overlay-start first) (overlay-end first)))
    (delete-overlay first)
    (oset obj overlays (cdr overlays))
    (dslide-marker obj end)
    ;; return progress
    start))

(cl-defmethod dslide-backward ((obj dslide-action-item-reveal))
  (when-let ((previous-item (dslide-section-previous obj 'item)))
    (oset obj overlays
          (cons (dslide-hide-element previous-item)
                (and (slot-boundp obj 'overlays)
                     (oref obj overlays))))
    (org-element-property :begin previous-item)))

;; ** Babel Action

;; TODO automatically map the blocks during begin and remove results... this is
;; kind of implemented but seems to inconsistently work.
;; TODO configure results removal behavior with an argument
;; TODO integrate with skipping with begin and end.
(defclass dslide-action-babel (dslide-action)
  () "Execute source blocks as steps.
By default blocks execute one by one with forward.  You can mark a block to
be special with the keyword:

- #+attr_dslide: begin

- #+attr_dslide: forward

- #+attr_dslide: backward

- #+attr_dslide: both

- #+attr_dslide: end

- #+attr_dslide: final

Other than both, which executes in either step direction,
these keywords correspond to the normal methods of the stateful
sequence class.  Blocks with method begin, end, and final are all
executed during the corresponding method and do not count as
steps.")

(cl-defmethod dslide--clear-results ((obj dslide-action-babel))
  (without-restriction
    (dslide-section-map
     obj 'src-block
     (lambda (e)
       (save-excursion
         (goto-char (org-element-property :begin e))
         (org-babel-remove-result-one-or-many nil))))))

(defun dslide--method-block-pred (method-names &optional unnamed)
  "Return a predicate to match the METHOD-NAMES.
Optional UNNAMED will return unnamed blocks as well."
  (lambda (block)
    (if-let* ((all-names (car (org-element-property
                               :attr_dslide block)))
              (names (string-split all-names)))
        (when (seq-intersection method-names names)
          block)
      (when unnamed
        block))))

(defun dslide--block-execute (block-element)
  (without-restriction
    (save-excursion
      (let ((block-begin (org-element-property :begin block-element))
            (block-end (org-element-property :end block-element)))
        (goto-char block-begin)
        ;; Executing babel seems to widen and also creates messages, and this
        ;; results in flashing.  The downside of just inhibiting re-display until
        ;; after the call is that if re-display is needed, such as if calling
        ;; `sleep-for' in a loop, then no updates will be visible.  However, the
        ;; user should really handle this with a timer or process output and
        ;; process sentinel etc.
        (condition-case user-wrote-flaky-babel
            ;; t for don't cache.  We likely want effects
            (progn (let ((inhibit-redisplay t))
                     (org-babel-execute-src-block t))
                   (dslide--base-buffer-highlight-region
                    block-begin block-end 'dslide-babel-success-highlight))
          ((debug error)
           (dslide--base-buffer-highlight-region
            block-begin block-end 'dslide-babel-error-highlight)
           ;; TODO consolidate moving the point & window points in base buffer
           (set-buffer (oref dslide--deck base-buffer))
           (goto-char block-begin)
           (if-let ((windows (get-buffer-window-list)))
               (progn
                 (mapc (lambda (w) (set-window-point w block-begin)) windows)
                 (select-window (car windows)))
             ;; TODO asking `y-or-n-p' defies the two-button interface
             (when (y-or-n-p "Block failed.  Visit failed block?")
               (switch-to-buffer (oref dslide--deck base-buffer))
               (goto-char block-begin)
               ;; TODO remove overlays after one command, like pulse
               (recenter)))
           ;; TODO option to try again / skip
           ;; TODO integrate with dslide--debug
           (error "Babel block at %s failed: %s"
                  (org-element-property :begin block-element)
                  user-wrote-flaky-babel)))))))

(cl-defmethod dslide--get-blocks ((obj dslide-action-babel) &optional method-name)
  "Return OBJ's block with keyword value METHOD-NAME.
The affiliated keywords look like:

#+attr_dslide: METHOD-NAME METHOD-NAME METHOD-NAME

The possible values for METHOD-NAME correspond to the
stateful-sequence class methods.  METHOD-NAME is a string."
  (let ((predicate (dslide--method-block-pred (list method-name))))
    (dslide-section-map obj 'src-block predicate)))

(cl-defmethod dslide-forward ((obj dslide-action-babel))
  (when-let* ((predicate (dslide--method-block-pred
                          '("forward" "both") t))
              (next (dslide-section-next obj 'src-block predicate)))
    (dslide--block-execute next)
    (org-element-property :begin next)))

(cl-defmethod dslide-backward ((obj dslide-action-babel))
  (when-let* ((predicate (dslide--method-block-pred
                          '("backward" "both")))
              (prev (dslide-section-previous obj 'src-block predicate)))
    (dslide--block-execute prev)
    (org-element-property :begin prev)))

(cl-defmethod dslide-begin ((obj dslide-action-babel))
  (when-let ((block-elements (dslide--get-blocks obj "begin")))
    (mapc #'dslide--block-execute block-elements)))

(cl-defmethod dslide-end ((obj dslide-action-babel))
  ;; Do not use the default implementation because it will play all blocks
  ;; forward.
  (dslide-marker obj (org-element-property :end (dslide-heading obj)))
  (when-let ((block-elements (dslide--get-blocks obj "end")))
    (mapc #'dslide--block-execute block-elements)))

(cl-defmethod dslide-final :after ((obj dslide-action-babel))
  (when-let ((block-elements (dslide--get-blocks obj "final")))
    (mapc #'dslide--block-execute block-elements)))

;; ** Image Action

(defclass dslide-action-image (dslide-action)
  ((kill-buffer
    :initform nil
    :initarg :kill-buffer
    :documentation "Kill the buffer.  Default nil just buries it.")
   (include-linked
    :initform t
    :initarg :include-linked
    :documentation "Loads linked images.  See `org-display-inline-images'.")
   (fullscreen
    :initform nil
    :initarg :fullscreen
    :documentation "Switch to full frame during display.")
   (hide-mode-line
    :initform t
    :initarg :hide-mode-line
    :documentation "Turn on `hide-mode-line-mode'.")
   (refresh
    :initform nil
    :initarg :refresh
    :documentation "Reload images.  See `org-display-inline-images'."))
  "Show images fullscreen in a buffer.")

(cl-defmethod dslide-begin :after ((obj dslide-action-image))
  (org-display-inline-images
   (oref obj include-linked)
   (oref obj refresh)
   (org-element-property :begin (dslide-heading obj))
   (org-element-property :end (dslide-heading obj))))

;; TODO implementation relies on org link opening.  Does not check for file or
;; check that image mode displays the link correctly.
;; TODO make it just a link action?
(cl-defmethod dslide-forward ((obj dslide-action-image))
  (when-let ((link (dslide-section-next obj 'link)))
    (dslide-push-window-config nil)
    ;; TODO success detection
    (let ((org-link-frame-setup '((file . find-file)))
          (display-buffer-overriding-action (when (oref obj fullscreen)
                                              '(display-buffer-full-frame))))
      (org-link-open link))

    (when (eq (buffer-local-value 'major-mode (current-buffer))
              'image-mode)
      (when (oref obj hide-mode-line)
        (when (require 'hide-mode-line nil t)
          (hide-mode-line-mode 1)))
      (image-transform-fit-to-window)
      (let ((image-buffer (current-buffer)))
        (dslide-push-step
         (lambda (_)
           (when (buffer-live-p image-buffer)
             (if (oref obj kill-buffer)
                 (kill-buffer image-buffer)
               (bury-buffer image-buffer)))))))
    (org-element-property :begin link)))

(cl-defmethod dslide-backward ((obj dslide-action-image))
  (when-let ((link (dslide-section-previous obj 'link)))
    (dslide-push-window-config nil)
    ;; TODO success detection
    (let ((org-link-frame-setup '((file . find-file)))
          (display-buffer-overriding-action (when (oref obj fullscreen)
                                              '(display-buffer-full-frame))))
      (org-link-open link))

    (when (eq (buffer-local-value 'major-mode (current-buffer))
              'image-mode)
      (when (oref obj hide-mode-line)
        (when (require 'hide-mode-line nil t)
          (hide-mode-line-mode 1)))
      (image-transform-fit-to-window)
      (let ((image-buffer (current-buffer)))
        (dslide-push-step
         (lambda (_)
           (when (buffer-live-p image-buffer)
             (if (oref obj kill-buffer)
                 (kill-buffer image-buffer)
               (bury-buffer image-buffer)))))))
    (org-element-property :begin link)))

;; * Child Actions
(defclass dslide-child-action (dslide-action) ()
  "Base class for child actions."
  :abstract t)

(cl-defmethod dslide-deck-forward-child ((obj dslide-action))
  "Advance OBJ's marker while returning next child heading.
Marker is moved to the end of OBJ's heading if no matching
child is found."
  (if-let* ((marker (dslide-marker obj))
            (heading (dslide-heading obj))
            (target-level (1+ (org-element-property :level heading)))
            (next (dslide--contents-map
                   heading 'headline
                   (lambda (child)
                     (and (= target-level (org-element-property :level child))
                          (> (org-element-property :begin child) marker)
                          child))
                   nil t)))
      (prog1 next
        (dslide-marker obj (org-element-property :begin next)))
    (dslide-marker obj (org-element-property :end (dslide-heading obj)))
    nil))

(cl-defmethod dslide-deck-backward-child ((obj dslide-action))
  "Advance OBJ's marker backward while returning previous child.
Marker is moved to the beginning of OBJ's heading if no matching
child is found."
  (if-let* ((marker (dslide-marker obj))
            (heading (dslide-heading obj))
            (target-level (1+ (org-element-property :level heading)))
            ;; We have to get all the children and find the last match
            (next (car
                   (last
                    (dslide--contents-map
                     heading 'headline
                     (lambda (child)
                       (and (= target-level (org-element-property :level child))
                            (< (org-element-property :begin child) marker)
                            child)))))))
      (prog1 next
        (dslide-marker obj (org-element-property :begin next)))
    (dslide-marker obj (org-element-property :begin (dslide-heading obj)))
    nil))

;; ** Default Child Action
(defclass dslide-child-action-slide (dslide-child-action)
  ((child
    :initform nil
    :documentation "Current child."))
  "Default child action.  Children are independent slides.")

(cl-defmethod dslide-forward ((obj dslide-child-action-slide))
  ;; For child slides, we make a slide out of the next child heading and advance
  ;; our progress forward to the end of that child
  (let (progress)
    (when-let ((child (oref obj child)))
      (setq progress (dslide-forward child))
      (unless progress
        (dslide-final child)
        (oset obj child nil)))
    (unless progress
      (when-let ((child (dslide-deck-forward-child obj)))
        ;; TODO transitive action customization
        (let ((child (dslide--make-slide child (oref dslide--deck slide))))
          (dslide-begin child)
          (oset obj child child))
        (setq progress (org-element-property :begin child))))
    progress))

(cl-defmethod dslide-backward ((obj dslide-child-action-slide))
  ;; For child slides, we make a slide out of the previous child heading and
  ;; advance our progress backward to the beginning of that child
  (let (progress)
    (when-let ((child (oref obj child)))
      (setq progress (dslide-backward child))
      (unless progress
        (dslide-final child)
        (oset obj child nil)))
    (unless progress
      (when-let ((child (dslide-deck-backward-child obj)))
        ;; TODO transitive action customization
        (let ((child (dslide--make-slide child (oref dslide--deck slide))))
          (dslide-end child)
          (oset obj child child))
        (setq progress (org-element-property :begin child))))
    progress))

(cl-defmethod dslide-end :after ((obj dslide-child-action-slide))
  (when-let ((child (dslide-deck-backward-child obj)))
    (let ((child (dslide--make-slide child (oref dslide--deck slide))))
      (prog1 (dslide-end child)
        (oset obj child child)))))

(cl-defmethod dslide-final :after ((obj dslide-child-action-slide))
  (when-let ((child (oref obj child)))
    (dslide-final child)))

;; ** Inline Child Action
;; While the basics of making a child out of the next heading are the same, an
;; action that controls children on its own does not return them to the deck.
;; It needs to update the buffer restriction as necessary, call lifecycle
;; functions, and pass through calls to step forward.

;; TODO round-robin child action
;; TODO every-child action

;; TODO override the child's own child action
(defclass dslide-child-action-inline (dslide-child-action)
  ((children
    :initform nil
    :documentation "Children that have been instantiated."))
  "Display children inline with the parent.")

(cl-defmethod dslide-forward ((obj dslide-child-action-inline))
  (let (progress exhausted)
    (while (not (or progress exhausted))
      ;; First try the most recently added child
      (setq progress (when-let* ((child (car (oref obj children))))
                       (dslide-forward child)))
      ;; If the child didn't make progress, try to load up the next child
      (unless progress
        (if-let* ((child-heading (dslide-deck-forward-child obj))
                  (child (dslide--make-slide
                          child-heading
                          (oref dslide--deck slide)
                          :slide-action #'dslide-action-narrow
                          :inline t
                          ;; TODO this won't compose at all
                          :slide-action-args '(:include-restriction t :with-children t)
                          :child-action 'none)))
            (progn (dslide-begin child)
                   (setq progress child)
                   (push child (oref obj children)))
          (setq exhausted t))))
    progress))

(cl-defmethod dslide-backward ((obj dslide-child-action-inline))
  (let (progress)
    (while (and (oref obj children) (not progress))
      ;; First try the most recently added child
      (setq progress (when-let* ((child (car (oref obj children))))
                       (dslide-backward child)))

      ;; If the child didn't make progress, narrow it away
      (unless progress
        (let* ((finished (pop (oref obj children)))
               (heading (dslide-heading finished)))
          (dslide-deck-backward-child obj)       ; for marker effects 💡
          ;; TODO do this with overlays in a nested child ☢️
          (when heading
            (narrow-to-region (point-min) (org-element-property :begin heading))
            (run-hooks 'dslide-narrow-hook))
          (dslide-final finished)
          (setq progress (car (oref obj children))))))
    progress))

(cl-defmethod dslide-end :after ((obj dslide-child-action-inline))
  (dslide-marker obj (org-element-property :begin (dslide-heading obj)))
  (let (exhausted)
    (while (not exhausted)
      ;; If the child didn't make progress, try to load up the next child
      (if-let* ((child-heading (dslide-deck-forward-child obj)))
          (let* ((child (dslide--make-slide
                         child-heading
                         (oref dslide--deck slide)
                         :inline t
                         ;; TODO this won't compose at all
                         :slide-action #'dslide-action-narrow
                         :slide-action-args '(:include-restriction t :with-children t)
                         :child-action 'none)))
            (let ((dslide-slide-in-effect nil))
              (dslide-end child))
            (push child (oref obj children)))
        (setq exhausted t)))))

(cl-defmethod dslide-final :after ((obj dslide-child-action-inline))
  (mapc #'dslide-final (oref obj children)))

;; * Filters

(defun dslide-built-in-filter (heading)
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

(defun dslide-hide-region (beg end &optional keep-fill)
  "Return overlay hiding region between BEG and END.
Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (let ((ov (make-overlay beg end)))
    (if keep-fill
        (let ((background (face-attribute 'default :background)))
          (overlay-put ov 'face `(:foreground ,background :background ,background)))
      (overlay-put ov 'display ""))
    ov))

(defun dslide-hide-element (element &optional keep-fill)
  "Return an overlay that will hide ELEMENT.
Element is an org element.Optional KEEP-FILL will obscure but not
change the contents of text, keeping its height and width for
filling in other content."
  (dslide-hide-region (org-element-property :begin element)
                  (org-element-property :end element)
                  keep-fill))

(defun dslide-hide-item (item &optional keep-fill)
  "Return an overlay that hides ITEM.
See `org-item-struct' for structure of ITEM.  Note, this hides
the entire item, which may contain sub-items, but revealing
children of a hidden parent doesn't really make sense.

Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (dslide-hide-region
   (car item) (car (last item)) keep-fill))

(defun dslide-hide-contents (element &optional keep-fill)
  "Return an overlay that hides the contents of ELEMENT.
Element is an org element.  You should probably not use this on
headings because their contents includes the sections and the
children.  See `dslide-hide-section' and `dslide-hide-children'.

Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (dslide-hide-region (org-element-property :contents-begin element)
                  (org-element-property :end element)
                  keep-fill))

(defun dslide-hide-section (heading &optional keep-fill)
  "Return an overlay that hides the section of HEADING.
HEADING is a headline type org element.

Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (dslide-hide-region
   (dslide--section-begin heading)
   (dslide--section-end heading)
   keep-fill))

(defun dslide-hide-children (heading &optional keep-fill)
  "Return an overlay that hides the children of HEADING.
HEADING is a headline type org element.

Optional KEEP-FILL will obscure but not change the contents of text, keeping
its height and width for filling in other content."
  (dslide-hide-region
   (dslide--section-end heading)
   (org-element-property :end heading)
   keep-fill))

;; * Element Mapping

;; Functions of headings are private so that corresponding slide methods can be
;; public.  Private methods with public counterparts are at least as stable as
;; the public method.

(defun dslide--map
    (element type fun &optional info first-match no-recursion)
  "Map FUN over the contents of the ELEMENT matching TYPE.
INFO, FIRST-MATCH, and NO-RECURSION are described in
`org-element-map'."
  (let ((type (if (listp type) type (list type))))
    (save-excursion
      (save-restriction
        (narrow-to-region (org-element-property :begin element)
                          (org-element-property :end element))
        (let ((data (org-element-parse-buffer)))
          (org-element-map data type fun info
                           first-match no-recursion))))))

(defun dslide--contents-map
    (element type fun &optional info first-match no-recursion)
  "Map FUN over the contents of the ELEMENT matching TYPE.
INFO, FIRST-MATCH, and NO-RECURSION are described in
`org-element-map'."
  (let ((type (if (listp type) type (list type))))
    (save-excursion
      (save-restriction
        (when-let ((begin (org-element-property :contents-begin element))
                   (end (org-element-property :contents-end element)))
          (narrow-to-region begin end)
          (let ((data (org-element-parse-buffer)))
            (org-element-map data type fun info
                             first-match no-recursion)))))))

(defun dslide--section-map
    (heading type fun &optional info first-match no-recursion)
  "Map FUN over elements matching TYPE in the SECTION of HEADING.
This includes all text up to the first child.  INFO, FIRST-MATCH,
and NO-RECURSION are described in `org-element-map'."
  (when-let ((section (dslide--section heading)))
    (dslide--map section type fun info
                 first-match no-recursion)))

(defun dslide--section-next
    (heading type after &optional pred info no-recursion)
  "Return HEADING's next element of TYPE that begins after AFTER.
PRED, INFO, FIRST-MATCH, and NO-RECURSION are described in
`org-element-map'."
  (let* ((combined-pred (dslide-and
                         pred
                         (lambda (e) (> (org-element-property :begin e) after)))))
    (dslide--section-map
     heading type combined-pred info t no-recursion)))

(defun dslide--section-previous
    (heading type before &optional pred info no-recursion)
    "Return HEADING's previous element of TYPE before BEFORE.
PRED, INFO, FIRST-MATCH, and NO-RECURSION are described in
`org-element-map'."
  (let* ((combined-pred (dslide-and
                         pred
                         (lambda (e) (< (org-element-property :begin e) before)))))
    ;; We can't map in reverse, so just retrieve all matched elements and
    ;; return the last one.
    (car (last (dslide--section-map
                heading type combined-pred info nil no-recursion)))))

(defun dslide--section (heading)
  "Get the section of a HEADING."
  (dslide--map
   heading 'section #'identity nil t t))

(defun dslide--section-begin (heading)
  "Always return a point, even if HEADING is empty."
  (if-let ((section (dslide--map
                     heading 'section #'identity nil t t)))
      (org-element-property :begin section)
    (or (org-element-property :contents-begin heading)
        (org-element-property :end heading))))

(defun dslide--section-end (heading)
  "Always return a point, even if HEADING is empty."
  (let ((not-self (lambda (e) (unless (equal (org-element-property :begin e)
                                        (org-element-property :begin heading))
                           e))))
    (if-let ((section-or-heading (dslide--map
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
(defun dslide--previous-sibling (heading &optional predicate)
  "Return the previous sibling HEADING if it exists.
PREDICATE should accept an ELEMENT argument and return non-nil."
  (without-restriction
    (save-excursion
      (goto-char (org-element-property :begin heading))
      (let* ((predicate (or predicate #'identity))
             found)
        (while (and (not (bobp))
                    (not found)
                    (org-get-previous-sibling))
          (let ((element (org-element-at-point)))
            (when (and (eq (org-element-type element) 'headline)
                       (funcall predicate element))
              (setq found element))))
        found))))

(defun dslide--next-sibling (heading &optional predicate)
  "Return the next sibling HEADING if it exists.
PREDICATE should accept an ELEMENT argument and return non-nil."
  (without-restriction
    (save-excursion
      (goto-char (org-element-property :begin heading))
      (let* ((predicate (or predicate #'identity))
             found)
        (while (and (not (eobp))
                    (not found)
                    (org-get-next-sibling))
          (let ((element (org-element-at-point)))
            (when (and (eq (org-element-type element) 'headline)
                       (funcall predicate element))
              (setq found element))))
        found))))

(defun dslide--list-item-contains (item loc)
  (when item
    (let ((beg (car item))
          (end (car (last item))))
      (and  (>= loc beg)
            (< loc end)))))

(defun dslide-type-p (element-or-type type)
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

(defun dslide--child-predicate (heading &optional predicate)
  "Create PREDICATE matching children of HEADING.
PREDICATE should return matching children."
  (let ((level (org-element-property :level heading))
        (predicate (or predicate #'identity)))
    (lambda (child)
      (and (= (1+ level) (org-element-property :level child))
           (funcall predicate child)
           child))))

;; TODO symbol bloat
(defun dslide--heading-p (element)
  (dslide-type-p element 'headline))

(defun dslide--element-root (element &optional type)
  "Get the root parent of ELEMENT of TYPE.
TYPE is a list or type symbol.  Parents not of TYPE will be
skipped and the last matching parent will be returned.  ELEMENT
could be the root."
  (let (found)
    (while element
      (when (or (not type) (dslide-type-p element type))
        (setq found element))
      (setq element (org-element-property :parent element)))
    found))

(defun dslide--document-first-heading ()
  "Return the first heading element in the buffer."
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

(defun dslide--root-heading-at-point (&optional point)
  "Return the root heading if the POINT is contained by one."
  (save-excursion
    (when point (goto-char point))
    (let* ((element (org-element-at-point))
           (parent (dslide--element-root element 'headline)))
      (or parent (dslide--any-heading)))))

(defun dslide--any-heading ()
  "Return any heading that can be found."
  (save-excursion
    (if (not (numberp (org-back-to-heading-or-point-min)))
        (org-element-at-point)
      (when (re-search-forward org-heading-regexp)
        (org-back-to-heading)
        (org-element-at-point)))))

(defun dslide-and (&rest predicates)
  "Combine PREDICATES for filtering elements.
Each predicate should take one argument, an org element."
  (lambda (element)
    (seq-reduce
     (lambda (begin pred)
       (when (or (not pred)
                 (and begin (funcall pred begin)))
         begin))
     predicates element)))

;; * Slide Header

;; These variables were brought forward from org-tree-slide.  There's not
;; sufficient reason to upgrade them to customize variables nor remove.

;; TODO these can be used across buffers when set before cloning indirect
;; buffers, but that's a coincidence, not necessarily a design choice.
(defvar-local dslide-title nil
  "Presentation title.
If you have \"#+title:\" line in your org buffer, it wil be used
as a title of the slide.  If the buffer has no \"#+title:\" line,
the name of current buffer will be displayed.")

(defvar-local dslide-email nil
  "Email address.
If you have \"#+email:\" line in your org buffer, it will be used
as an address of the slide.")

(defvar-local dslide-author nil
  "Author name.
If you have \"#+author:\" line in your org buffer, it will be
used as a name of the slide author.")

(defvar-local dslide-date nil
  "Date.
If you have \"#+date:\" line in your org buffer, it will be used
as the date.")

;; TODO make public
;; TODO allow header override function
(defun dslide--make-header (&optional no-breadcrumbs)
  "Draw a header for the first tree in the restriction.
Set optional NO-BREADCRUMBS to non-nil to skip breadcrumbs.  The implementation
assumes the buffer is restricted and that there is a first tree."
  (dslide--delete-header)

  ;; Use of point-min is an implementation assumption, that the header is always
  ;; at the very top of the narrowed region and never wanted anywhere else.
  (setq dslide--header-overlay
        (make-overlay (point-min) (+ 1 (point-min))))

  (let* ((keywords (org-collect-keywords
                    '("TITLE" "EMAIL" "AUTHOR" "DATE")))
         (title (or dslide-title
                    (cadr (assoc-string "TITLE" keywords))
                    (buffer-name)))
         (author (or dslide-author
                     (cadr (assoc "AUTHOR" keywords))))
         (date (or dslide-date
                   (cadr (assoc-string "DATE" keywords))
                   (format-time-string "%Y-%m-%d")))
         (email (when-let ((email (or dslide-email
                                      (cadr (assoc-string "EMAIL" keywords)))))
                  (concat "<" email ">"))))

    ;;  The calls to `propertize' make up for the fact that these values may be
    ;;  strings, set from elsewhere, but we want to display these strings as if
    ;;  they were fontified within the buffer.
    (if dslide-header
        (overlay-put
         dslide--header-overlay 'before-string
         (concat (dslide--margin-lines dslide-margin-title-above)
                 (propertize title 'face 'org-document-title)
                 (dslide--margin-lines dslide-margin-title-below)
                 (when (and  dslide-header-date date)
                   (dslide--info-face (concat date "  ")))
                 (when (and  dslide-header-author author)
                   (dslide--info-face (concat author "  ")))
                 (when (and  dslide-header-email email)
                   (dslide--info-face (concat email "  ")))
                 (when (and (not no-breadcrumbs)
                            dslide-breadcrumb-separator)
                   (concat (dslide--info-face "\n")
                           (dslide--get-parents
                            dslide-breadcrumb-separator)))
                 (dslide--margin-lines dslide-margin-content)))

      (overlay-put dslide--header-overlay 'before-string
                   (dslide--margin-lines dslide-margin-content)))))

(defun dslide--info-face (s)
  (propertize s 'face 'org-document-info))

(defun dslide--margin-lines (lines)
  (dslide--info-face
   (if (display-graphic-p)
       (propertize "\n" 'line-height (float lines))
     (make-string (floor lines) ?\n))))

(defun dslide--breadcrumbs-reducer (delim)
  (lambda (previous next)
    (if (not previous) next
      (let ((props (text-properties-at (1- (length previous)) previous)))
        (concat previous (apply #'propertize delim props)
                next)))))

;; TODO use element API
(defun dslide--get-parents (delim)
  "Get parent headings and concat them with DELIM."
  ;; The implementation here uses the regex & point-based techniques so that
  ;; we're extracting buffer strings, which saves us from having to re-style
  ;; them to match whatever is in the buffer.
  (save-excursion
    (goto-char (point-min))
    (save-restriction
      (widen)
      (let ((parents nil)
            (reducer (dslide--breadcrumbs-reducer delim)))
        (while (org-up-heading-safe)
          (push (org-get-heading
                 'no-tags
                 dslide-breadcrumbs-hide-todo-state)
                parents))
        (let ((breadcrumbs (seq-reduce reducer parents nil)))
          (when dslide-breadcrumb-face
            (add-face-text-property 0 (length breadcrumbs)
                                    dslide-breadcrumb-face
                                    nil
                                    breadcrumbs))
          breadcrumbs)))))

(defun dslide--delete-header ()
  "Delete header."
  (when dslide--header-overlay
    (delete-overlay dslide--header-overlay)))

;; * Animation

;; TODO move respect for animation variables into this function
;; TODO END is a redundant argument unless a virtual newline is introduced.
;; Test if an overlay can can work via after-string.
;; TODO Support non-graphical
;; TODO Inline animation fallback, uncover text character by character.
;; TODO User-provided animation override function
(defun dslide-animation-setup (beg end)
  "Slide in the region from BEG to END.
Everything after BEG will be animated.  The region between BEG
and the value of `point-max' should contain a newline somewhere."
  (dslide--ensure-slide-buffer)
  (let* ((timer (timer-create))
         (goal-time (time-add (current-time)
                              dslide-animation-duration))
         (newline-region (save-match-data
                           (save-excursion
                             (goto-char beg)
                             (if (re-search-forward "\n" end t)
                                 (list (match-beginning 0)
                                       (match-end 0))
                               (error "No newline in region")))))
         (overlay (apply #'make-overlay newline-region))
         (initial-line-height
          (or (plist-get
               (text-properties-at (car newline-region))
               'line-height)
              1.0)))
    (push timer dslide--animation-timers)
    (push overlay dslide--animation-overlays)
    (timer-set-time timer (current-time)
                    dslide-animation-frame-duration)
    (timer-set-function timer #'dslide--animate
                        (list timer goal-time overlay initial-line-height))
    (timer-activate timer)))

(defun dslide--animate (timer goal-time overlay initial-line-height)
  (if (time-less-p goal-time (current-time))
      (progn (cancel-timer timer)
             (setq dslide--animation-timers
                   (delq timer dslide--animation-timers))
             (delete-overlay overlay)
             (setq dslide--animation-overlays
                   (delq overlay dslide--animation-overlays)))
    (let* ((diff (time-to-seconds (time-subtract goal-time (current-time))))
           (fraction (expt (/ diff dslide-animation-duration) 5.0))
           (lines dslide-slide-in-blank-lines)
           (line-height (* (+ initial-line-height lines)
                           fraction)))
      (overlay-put overlay 'line-height line-height))))

(defun dslide--animation-cleanup ()
  (while dslide--animation-timers
    (cancel-timer (pop dslide--animation-timers)))
  (while dslide--animation-overlays
    (delete-overlay (pop dslide--animation-overlays))))

;; * Assorted Implementation Details

(defun dslide--debug (slide &optional situation)
  (when dslide--debug
    (let* ((heading (dslide-heading slide))
           (headline-begin (org-element-property :begin heading))
           (headline-end (or (org-element-property :contents-begin heading)
                             (org-element-property :end heading)))
           (situation (or situation
                          "dslide--debug")))
      (message "%s begin: %s heading: %s"
               situation
               (marker-position (oref slide begin))
               (save-restriction
                 (widen)
                 (buffer-substring headline-begin (1- headline-end)))))))

(defun dslide--cleanup-state ()
  "Clean up states between contents and slides."
  (dslide--delete-header)
  (dslide--delete-overlays)
  (dslide--animation-cleanup)
  (mapc (lambda (f) (funcall f nil))
        (oref dslide--deck step-callbacks))
  (oset dslide--deck step-callbacks nil)
  (remove-hook 'post-command-hook #'dslide--contents-hl-line t))

(defun dslide--ensure-deck ()
  "Prepare for starting the minor mode.
Call this when writing commands that could be called before or
after a deck exists but should create a deck if it does not exist.

In functions that should only be called when a deck is alive and
associated with the current buffer, use `dslide-live-p'
and throw an error if it's not live.

This function sets up the deck.  Many operations such as calling
hooks must occur in the deck's :slide-buffer."
  (unless (dslide-live-p)
    ;; Prevent starting within indirect buffers
    (when (buffer-base-buffer (current-buffer))
      (error "Buffer is indirect but deck is already live"))

    ;; TODO check assumed initial conditions
    (let* ((base-buffer (current-buffer))
           (slide-buffer-name (format "*deck: %s*" (buffer-name
                                                    base-buffer))))

      ;; stale buffers likely indicate an issue
      (when-let ((stale-buffer (get-buffer slide-buffer-name)))
        (display-warning '(dslide dslide--ensure-deck)
                         "Stale deck buffer was killed")
        (kill-buffer slide-buffer-name))

      (let* ((class (or (intern-soft (dslide--keyword-value
                                      "DECK_CLASS"))
                        dslide-default-deck-class
                        'dslide-deck))
             (window-config (current-window-configuration))

             (slide-buffer (clone-indirect-buffer
                            slide-buffer-name
                            nil))
             (deck (apply class
                          :base-buffer base-buffer
                          :slide-buffer slide-buffer
                          :window-config window-config
                          nil)))
        (setq dslide--deck deck)
        (display-buffer slide-buffer dslide--display-actions)
        (set-buffer slide-buffer)

        (widen)
        (org-fold-show-all)
        ;; Enter the state model
        (dslide--choose-slide deck
                          dslide-start-from)))))

(defun dslide--showing-contents-p ()
  "Return t if current buffer is displaying contents."
  (and dslide--deck
       (eq (current-buffer) (oref dslide--deck slide-buffer))
       (eq 'contents (oref dslide--deck slide-buffer-state))))

(defun dslide--showing-slides-p ()
  "Return t if current buffer is displaying contents."
  (and dslide--deck
       (eq (current-buffer) (oref dslide--deck slide-buffer))
       (eq 'slides (oref dslide--deck slide-buffer-state))))

(defun dslide--delete-overlays ()
  "Delete content overlays."
  (while dslide--overlays
    (delete-overlay (pop dslide--overlays)))
  (while dslide--step-overlays
    (delete-overlay (pop dslide--step-overlays)))
  (when dslide--contents-hl-line-overlay
    (delete-overlay dslide--contents-hl-line-overlay))
  (setq dslide--contents-hl-line-overlay nil))

(defun dslide--ensure-slide-buffer (&optional display)
  "Run in commands that must run in the slide buffer.
Unless optional DISPLAY is non-nil, the buffer is only set."
  (unless (dslide-live-p)
    (error "Live deck not found"))
  (if display
      (display-buffer (oref dslide--deck slide-buffer)
                      dslide--display-actions)
    (set-buffer (oref dslide--deck slide-buffer))))

(defun dslide--keyword-value (key)
  "Get values like #+KEY from document keywords."
  (cadr (assoc-string key (org-collect-keywords `(,key)))))

(defun dslide--feedback (key)
  "Show feedback message for KEY.
See `dslide-feedback-messages'.  This provides Explicit feedback
for commands without visible side effects."
  (when-let ((feedback (plist-get dslide-feedback-messages
                                  key)))
    (let ((message-log-max nil))
      (message "%s" feedback))))

;; TODO these could check for inheritance from some base class, which would save
;; people who write action names in the class property etc.
(defun dslide--classes (class-names)
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
                   '(dslide
                     dslide-class
                     dslide-filter)
                   (format "Class name not a class: %s" name))))))

(defun dslide--filter (filter-name)
  "FILTER-NAME is a string that might contain a filter name."
  (when-let ((symbol (or (when (symbolp filter-name)
                           filter-name)
                         (intern-soft filter-name))))
    (if (functionp symbol)
        symbol
      (display-warning
       '(dslide
         dslide-class
         dslide-filter)
       (format "Filter name not a function: %s" filter-name)))))

(defun dslide--parse-class-with-args (property-data)
  (unless (string= "nil" property-data)
    (let ((classes-with-args
           (dslide--parse-classes-with-args property-data)))
      (prog1 (car classes-with-args)
        (unless (= 1 (length classes-with-args))
          (display-warning '(dslide)
                           (format "Only one classes allowed: %s"
                                   (cdr classes-with-args))))))))

(defun dslide--keyword-symbol-p (string)
  (eq 0 (string-match-p ":\\(?:\\sw\\|\\s_\\)+$" string)))

(defun dslide--parse-classes-with-args (property-data)
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
                      (class (dslide--class token t)))
            ;; peak for a key to decide if we continue parsing as args go back
            ;; to parsing as class names
            (push class class-with-args)
            (while-let ((token (car tokens))
                        (tokenp (dslide--keyword-symbol-p token)))
              ;; `intern-soft' only creates the symbol if it corresponds to an
              ;; existing class (among other things)
              (push (intern-soft (pop tokens)) class-with-args)
              (let ((val (pop tokens)))
                (push (car (read-from-string val)) class-with-args)))
            (push (reverse class-with-args) classes-with-args)
            (setq class-with-args nil))
        (wrong-type-argument
         (display-warning
          '(dslide) (cdr err))))
      (reverse classes-with-args))))

;; This should not interpret nil's specially because that should he handled
;; upstream by the parse functions
(defun dslide--class (class-name &optional signal)
  "CLASS-NAME is a string or symbol that should be a class name.
Optional SIGNAL if you want to process `wrong-type-argument' in
the caller."
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
         '(dslide)
         (format "Class name not a class: %s" class-name))
        nil))))

;; * Contents Highlight Line

;; Basically we need to use the post-command hook to update a line with our
;; handy-dandy face.  This is basically just a less feature-ful re-implementation
;; of hl-line.  hl-line is kind of subtle and works across all buffers.  This is
;; safer and defaults to the face attribute, :inverse-video, which is super high
;; contrast, good when navigating slide headlines like a menu.
(defun dslide--contents-hl-line ()
  (unless dslide--contents-hl-line-overlay
    (setq dslide--contents-hl-line-overlay
          (make-overlay (point) (point)))
    (overlay-put dslide--contents-hl-line-overlay
                 'face 'dslide-contents-selection-face)
    (overlay-put dslide--contents-hl-line-overlay
                 'priority 10))
  (when-let ((element (org-element-at-point)))
    (setf (overlay-start dslide--contents-hl-line-overlay)
          (org-element-property :begin element))
    (setf (overlay-end dslide--contents-hl-line-overlay)
          (org-element-property :end element))))

;; * Lifecycle

(defvar-keymap dslide-mode-map
  :doc "The keymap for `dslide-mode'."
  "<left>" #'dslide-deck-backward
  "<right>" #'dslide-deck-forward
  "<up>" #'dslide-deck-start
  "<down>" #'dslide-deck-stop)

;;;###autoload
(define-minor-mode dslide-mode
  "A presentation tool for Org Mode."
  :init-value nil
  :interactive nil
  :keymap dslide-mode-map
  :group 'dslide
  :global t
  (unless (eq 'org-mode (buffer-local-value
                         'major-mode (current-buffer)))
    (user-error "Not an org buffer")
    (dslide-mode -1))
  (cond (dslide-mode
         ;; Create the indirect buffer and link it via the deck object.
         (dslide--ensure-deck)
         (funcall (or dslide-start-function #'dslide-display-slides))
         (run-hooks 'dslide-start-hook)
         (dslide--feedback :start))
        (t
         (dslide--stop))))

(defun dslide-live-p ()
  "Check if a deck is associated so that commands can complete."
  (and dslide-mode
       dslide--deck
       (dslide-deck-live-p dslide--deck)))

;; TODO rename these functions to `switch-to'?
(defun dslide-display-slides ()
  (dslide--ensure-slide-buffer t)
  (dslide--cleanup-state)
  (oset dslide--deck slide-buffer-state 'slides)
  (widen)
  (org-fold-show-all)
  (dslide-begin dslide--deck))

(defun dslide--base-buffer-highlight-region (beg end &optional face)
  "Pulse region between BEG and END in base buffer.
Optional FACE defaults to `dslide-highlight'."
  (unless (dslide-live-p)
    (error "Live deck not found"))
  (let ((buffer (current-buffer))
        (face (or face 'dslide-highlight)))
    (set-buffer (oref dslide--deck base-buffer))
    (let ((overlay (make-overlay beg end)))
      (overlay-put overlay 'face face)
      (push overlay dslide--step-overlays))
    (set-buffer buffer)))

(defun dslide--base-buffer-highlight-line (&optional pos face)
  "Highlight line containing POS or current point.
Optional FACE defaults to `dslide-highlight'."
  (unless (dslide-live-p)
    (error "Live deck not found"))
  (let ((buffer (current-buffer))
        (face (or face 'dslide-highlight)))
    (set-buffer (oref dslide--deck base-buffer))
    (save-excursion
      (when pos (goto-char pos))
      (let* ((beg (progn (vertical-motion 0) (point)))
             (end (progn (vertical-motion 1) (point)))
             (overlay (make-overlay beg end)))
        (overlay-put overlay 'face face)
        (push overlay dslide--step-overlays)))
    (set-buffer buffer)))

(defun dslide--follow (progress)
  "Set the base buffer window point to PROGRESS.
PROGRESS must be an integer buffer location, not a marker."
  (unless (dslide-live-p)
    (error "Live deck not found"))
  (let ((pos (cond ((integerp progress) progress)
                   ((eieio-object-p progress)
                    (marker-position (oref progress begin)))
                   ((markerp progress) (marker-position progress)))))
    (when (and (not (booleanp progress)) (null pos))
      (message "Incomprehensible progress reported: %s" progress))
    (when (and pos dslide-base-follows-slide)
      (set-buffer (oref dslide--deck base-buffer))
      (unless (and (>= pos (point-min))
                   (<= pos (point-max)))
        (widen))
      (goto-char pos)
      (dslide--base-buffer-highlight-line)
      ;; TODO maybe only two of these are actually necessary
      (org-fold-show-context)
      (org-fold-show-entry)
      (org-fold-show-subtree)
      (when-let ((windows (get-buffer-window-list (current-buffer))))
        (mapc (lambda (w) (set-window-point w pos)) windows))
      (set-buffer (oref dslide--deck slide-buffer)))))

(defun dslide-display-contents ()
  "Switch to showing contents in the slide buffer.
This is a valid `dslide-start-function' and will start
each slide show from the contents view."
  (dslide--ensure-slide-buffer t)
  (dslide--cleanup-state)
  (oset dslide--deck slide-buffer-state 'contents)

  (widen)
  (org-overview)
  (recenter)

  (when dslide-header
    (if-let ((first (dslide--document-first-heading)))
        (narrow-to-region (org-element-property :begin first)
                          (point-max))
      ;; No first heading.  Just header.  Empty contents.
      (narrow-to-region (point-max)
                        (point-max)))
    (run-hooks 'dslide-narrow-hook)
    (dslide--make-header t))

  (when dslide-contents-selection-highlight
    (add-hook 'post-command-hook #'dslide--contents-hl-line nil t))

  ;; TODO walk all headings with the filter and add overlays on the hidden stuff
  ;; TODO filter slides that don't have a display action?

  (dslide--feedback :contents)
  (run-hooks 'dslide-contents-hook))

(defun dslide--stop ()
  "Stop the presentation entirely.
Kills the indirect buffer, forgets the deck, and displays the
source buffer."
  (interactive)
  (when-let* ((deck dslide--deck)
              (slide-buffer (oref deck slide-buffer))
              (base-buffer (oref deck base-buffer)))
    (with-demoted-errors "Deck finalization failed: %s"
      (dslide-final dslide--deck))
    ;; Animation timers especially should be stopped
    (dslide--cleanup-state)
    (when slide-buffer
      (kill-buffer slide-buffer))
    (display-buffer base-buffer dslide--display-actions)
    (set-buffer base-buffer)
    (recenter (window-height))
    (setq dslide--deck nil)
    (when dslide-mode
      (dslide-mode -1))
    (run-hooks 'dslide-stop-hook)
    (dslide--feedback :stop)))

;; * User Commands

;;;###autoload
(defun dslide-deck-stop ()
  "Stop the presentation.
It is recommended to not bind this to a controller button unless
you have five buttons or will use the display button to stop and
can reliably select displays via other means."
  (interactive)
  (dslide--stop))

;; TODO make secondary actions supported
;;;###autoload
(defun dslide-deck-start ()
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
  (if (dslide-live-p)
      (progn (dslide--ensure-slide-buffer)
             (if (dslide--showing-slides-p)
                 ;; TODO check for secondary task here
                 (dslide-display-contents)
               (dslide--choose-slide dslide--deck 'contents)
               (dslide-display-slides)))
    (let ((dslide-start-function
           #'dslide-display-slides))
      (dslide-mode 1))))

;;;###autoload
(defun dslide-deck-forward ()
  "Advance slideshow forward."
  (interactive)
  (dslide--ensure-slide-buffer)
  (if (dslide--showing-contents-p)
      (org-next-visible-heading 1)
    (dslide--ensure-slide-buffer)
    (dslide-forward dslide--deck)))

;;;###autoload
(defun dslide-deck-backward ()
  "Advance slideshow backward."
  (interactive)
  (dslide--ensure-slide-buffer)
  (if (dslide--showing-contents-p)
      (org-previous-visible-heading 1)
    (dslide--ensure-slide-buffer)
    (dslide-backward dslide--deck)))

(provide 'dslide)

;;; dslide.el ends here
