;;; dslide.el --- Domain Specific sLIDEs. A presentation framework -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2011-2023 Takaaki ISHIKAWA
;; Copyright (C) 2024 Positron
;;
;; Author: Positron <contact@positron.solutions>
;; Version: 0.5.3
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

;; DSL IDE builds presentations on top of org mode documents.  It integrates
;; with arbitrary buffers and org babel, making anything Emacs can do easy to
;; incorporate into a presentation.  Headers are configured with extensible
;; actions.  Custom steps can be scripted with babel blocks or made into
;; reusable custom actions.  DSL IDE achieves a good result with no preparation
;; but can achieve anything Emacs can display if you need it to.
;;
;; See the README for more detailed instructions.
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
;; 2. Org element mapping implementations that are private but exposed publicly
;; on slide actions and elsewhere because they are super useful.
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
;; Also pay very close attention to `dslide-stateful-sequence' and how sequences
;; and steps can be pushed.
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
  :type 'boolean)

(defcustom dslide-start-from 'point
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
                 (const :tag "Slide at point" point)))

(defcustom dslide-start-function #'dslide-display-slides
  "When starting the mode, this is the default starting function.
It should usually call `dslide-display-slides' or
`dslide-display-contents'.  You can build commands that
use `let' binding to temporarily set this variable in order to
start with a specific starting function."
  :type 'function)

(defcustom dslide-header t
  "The status of displaying the slide header."
  :type 'boolean)

(defcustom dslide-header t
  "Display header in contents buffer.
When this is disabled, the keywords for title etc will remain
visible, albeit scrolled away because of how `org-overview'
works."
  :type 'boolean)

(defcustom dslide-header-fun nil
  "Custom function to override heading generation.
The function you define should accept two arguments:

- CLEANUP: meaning to delete any state that was created for an
  existing header.

- optional BREADCRUMBS: indicating if creating breadcrumbs is
  appropriate or not, such as when displaying the contents view.

🚧 This option is experimental and the signature is subject to
change.

When nil, the header is generated by the default
`dslide-make-header', which respects many customize options.
However, you may find it faster to completely replace this
function to get exactly what you want."
  :type 'function)

(defcustom dslide-header-author t
  "Show the email in the header.
If there is a #+author: keyword, it will be used."
  :type 'boolean)

(defcustom dslide-header-email t
  "Show the email in the header.
If there is a #+email: keyword, it will be used."
  :type 'boolean)

(defcustom dslide-header-date t
  "Show the date in the header.
If there is a #+date: keyword, it will be used.
The current time will be used as a fallback."
  :type 'boolean)

(defcustom dslide-margin-title-above 0.5
  "Margin between header title and the top of the window.
Can be a float or integer."
  :type 'number)

(defcustom dslide-margin-title-below 0.5
  "Margin between title and other header info.
Can be a float or integer."
  :type 'number)

(defcustom dslide-margin-content 1.5
  "Margin between the slide header and its content.
Can be a float or integer."
  :type 'number)

(defcustom dslide-slide-in-effect t
  "Using a visual effect of slide-in for displaying trees."
  :type 'boolean)

(defcustom dslide-slide-in-blank-lines 15
  "Line height of the slide-in effect."
  :type 'number)

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
  :options '((:start string)
             (:forward string)
             (:backward string)
             (:contents string)
             (:stop string)
             (:after-last-slide string)))

(defcustom dslide-breadcrumb-face '(:inherit org-level-8)
  "Face added to the list of faces for breadcrumbs.
This can be a face name symbol or an anonymous face spec.  It
will be added to the face list, meaning it the original face's
properties remain unless shadowed."
  :type 'face)

(defcustom dslide-breadcrumb-separator " 🢒 "
  "Delimiter for breadcrumbs or nil to turn off breadcrumbs."
  :type '(choice (const :tag "Don't display breadcrumbs" nil)
                 (string :tag "Delimiter")))

(defcustom dslide-breadcrumbs-hide-todo-state t
  "If non-nil, hide TODO states in the breadcrumbs."
  :type 'boolean)

(defcustom dslide-animation-duration 1.0
  "How long slide in takes."
  :type 'number)

(defcustom dslide-animation-frame-duration (/ 1.0 60.0)
  "Length between updates.
Increase if your so-called machine has trouble drawing."
  :type 'number)

(defcustom dslide-start-hook nil
  "Runs after the slide buffer is created but before first slide.
Buffer is widened and fully visible."
  :type 'hook)

(defcustom dslide-stop-hook nil
  "Runs in the base buffer after stopping."
  :type 'hook)

(defcustom dslide-narrow-hook nil
  "Runs whenever the slide buffer restriction is updated.
Use this hook for behaviors that affect the displayed region.
Slides and sequences that do not display themselves or only
affect display in another buffer will not trigger this hook."
  :type 'hook)

(defcustom dslide-contents-hook nil
  "Runs last after switching to contents."
  :type 'hook)

(defcustom dslide-after-last-slide-hook '()
  "Run when forward is called but there is no next slide.
This can either provide feedback or quit immediately etc.
Consider using `dslide-push-step' and writing a callback that
only reacts to the `forward' state.  This callback will then only
run if the user immediately calls `dslide-deck-forward'
again.  `dslide-deck-stop' is another good choice."
  :type 'hook)

(defcustom dslide-default-slide-action #'dslide-slide-action-child
  "Action class with lifecycle around the section actions.
When stepping forward or backward, it is called before any
section action.  It's normal purpose is to update the buffer
restriction before section actions are run.

You can configure this per-heading by setting the
SLIDE_ACTION keyword.  You can configure it for
the document default by adding an SLIDE_ACTION
keyword."
  :type 'function)

;; TODO test the use of plist args
(defcustom dslide-default-actions '(dslide-action-hide-markup)
  "Actions that run within the section display action lifecycle.
It's value is a list of symbol `dslide-action' sub-classes or (CLASS . ARGS)
forms where ARGS is a plist.  Each subclass will be instantiated
into an action object.  See the symbol `dslide-action' class and its methods
to learn about writing custom actions.

Many section actions are no-op whenever the content doesn't
contain any elements they act on.  You can add classes to this
list in order to have default behaviors for some org elements.

You can configure this per-heading by setting the DSLIDE_ACTIONS
keyword.  You can configure it for the document default by adding
an DSLIDE_ACTIONS keyword."
  :type '(list function))

(defcustom dslide-default-class 'dslide-slide
  "A class to more deeply modify slide behavior.
Value should be a custom class extending `dslide'.  You can
override methods if the built-in implementation is insufficient.
Consider upstreaming changes.

You can configure this per heading by setting the DSLIDE_CLASS
property.  You can configure it for the document default by
adding an DSLIDE_CLASS keyword."
  :type 'symbol)

(defcustom dslide-default-deck-class 'dslide-deck
  "A class to more deeply modify overall deck behavior.
Value should be a custom class extending symbol `dslide-deck'.
Use this to modify the root-level behaviors, including switching
to children and finding siblings.  You can configure this for the
document by adding the DSLIDE_ROOT_CLASS keyword."
  :type 'symbol)

(defcustom dslide-default-filter #'dslide-built-in-filter
  "A function used to call next on children.
The function used as actions should accept an org element, a
`headline' type element and return it if it is a valid heading or
return nil if it should be skipped.

You can configure this per heading by setting the DSLIDE_FILTER
keyword.  You can configure it for the document default by adding
an DSLIDE_FILTER keyword."
  :type 'function)

;; TODO can also probably be objects.  Use case?
(defcustom dslide-hide-markup-types '(comment
                                      comment-block
                                      drawer
                                      property-drawer
                                      keyword)
  "Default types to be hidden by `dslide-action-hide-markup'.
Can be any element in `org-element-all-elements'."
  :type '(repeat symbol))

(defcustom dslide-contents-selection-highlight t
  "Show a highlight on the selected headline.
This is useful if you have some subtle cursor feature enabled for
your presentation and wouldn't otherwise know what line you are
on in the contents view.  The default is also just a way more
obvious display style."
  :type 'boolean)

(defface dslide-contents-selection-face
  '((t :inherit org-level-1 :inverse-video t :extend t))
  "Face for highlighting the current slide root.")

(defface dslide-highlight
  '((t :inherit hl-line))
  "Face used in base buffer to highlight progress.
See `dslide-base-follows-slide'.")

(defface dslide-babel-success-highlight
  '((t :inherit hl-line))
  "Temporarily highlight babel blocks that succeeded.")

(defface dslide-babel-error-highlight
  '((t :inherit error))
  "Temporarily highlight babel blocks that failed.")

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

(defvar dslide-overlays nil
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
;; - `dslide-deck': is the first thing called into by `dslide-deck-forward' and
;;   `dslide-deck-backward'.  It chooses root headings to hydrate as slides and forwards
;;   these commands into the slides.
;;
;; - `dslide-slide': interprets an org heading into some actions and coordinates
;;   forwarding calls into actions in the correct order.  Through their actions,
;;   slides may forward calls into other slides.
;;
;; - `dslide-action': does most of the actual work of narrowing, hiding,
;;   animating, executing babel etc.  Actions with `slide-action' in their name
;;   likely hydrate child slides and forward calls into them.

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
`dslide-deck-backward' to `dslide-deck-forward', resulting in a
sequence that always starts at the beginning and always proceeds
to the end.  For a single step sequence that has identical effect
in both directions, this is appropriate.

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
progress exceeds POINT.  Usually, slide actions will be
responsible for determining if the POINT belongs to this slide or
one of its child slides, and the slide will just ask the child
action.")

;; ** Stateful Sequence
(defclass dslide-stateful-sequence () ()
  "An interface definition for linear sequences of steps.
This is an abstract class.

The sequence can be traversed forwards and backward.  `begin' and
`foward' are conjugates of `end' and 'backward'.

Because the sequence steps may rely on some setup and should
perform necessary teardown, the stateful sequence provides `begin'
`end' and `final' methods.

It can also be indexed by high-level navigation commands.  The
implementation of `dslide-goto' Sequences can run as
sub-sequences, where one sequence calls into another.  🚧 This
capability is largely unimplemented, but compatible with existing
work.

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

  (let ((inhibit-redisplay t)
        (old-point-min (point-min))
        progress
        reached-end)
    (while dslide--step-overlays
      (delete-overlay (pop dslide--step-overlays)))

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
      (dslide--follow progress (not (= old-point-min (point-min)))))

    (when reached-end
      (dslide--feedback :after-last-slide)
      (run-hooks 'dslide-after-last-slide-hook))))

(cl-defmethod dslide-backward ((obj dslide-deck))
  (unless (oref obj slide)
    ;; Calls implied from other commands should have started the lifecycle
    ;; already
    (error "No slide selected"))

  ;; Going backward is almost the same as going forward.  The big difference is
  ;; that when a slide is instantiated, it needs to be sent to its end.  Usually
  ;; the default implementation, which calls forward until progress is
  ;; exhausted, is fine.  Certain actions with side-effects may not like this,
  ;; and they should implement an actual `dslide-end' method as well as
  ;; idempotent `dslide-begin' and `dslide-final' if any support for going
  ;; backwards is desirable.

  (let ((inhibit-redisplay t)
        (old-point-min (point-min))
        progress
        reached-beginning)
    (while dslide--step-overlays
      (delete-overlay (pop dslide--step-overlays)))

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
           (dslide--follow progress (not (= old-point-min (point-min)))))
          (reached-beginning
           (user-error "No more previous slides!")))))

(cl-defmethod dslide--choose-slide ((obj dslide-deck) how)
  "Set the current slide of OBJ, according to HOW."
  ;; TODO apply filter when choosing starting slide
  (pcase how
    ('first (oset obj slide (dslide--make-slide
                             (dslide--document-first-heading))))
    ('contents (oset obj slide (dslide--make-slide
                                (dslide--root-heading-at-point (point)))))
    ('point
     (let ((base-point (with-current-buffer (oref obj base-buffer)
                         (point))))
       ;; TODO implement looking inside the slides using `goto' and recover
       ;; the child with a point argument.
       (oset obj slide
             (dslide--make-slide
              (dslide--root-heading-at-point base-point)))))))

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
     (lambda (_) (prog1 step (set-window-configuration window-config))))))

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
    :documentation "Outermost action that sets up for section actions.
Typically narrows, shows the header, creates slides from child
headings etc.  Because narrowing and children are necessarily
coupled, the slide action controls how children slides are
created.  See `dslide-default-slide-action'.")
   (section-actions
    :initform nil :initarg :section-actions
    :documentation "Typical actions that work on the section.
Live within slide action lifecycle.  See
`dslide-default-actions'.")
   (begin
    :initform nil :initarg :begin
    :documentation "Marker for retrieving this heading's org element."))
  "Stores and coordinates the actions of a heading.
The life-cycles of actions that run for a heading overlap, and
the slide object coordinates this overlap.  It delegates the
`stateful-sequence' calls into the actions in the appropriate
order.")

(cl-defmethod dslide-begin ((obj dslide-slide))
  (prog1 (when-let ((slide-action (oref obj slide-action)))
           (dslide-begin slide-action))
    (mapc #'dslide-begin (oref obj section-actions))))

(cl-defmethod dslide-end ((obj dslide-slide))
  (prog1 (when-let ((slide-action (oref obj slide-action)))
           (dslide-end slide-action))
    ;; Fairly certain the ordering of slide and section actions won't normally
    ;; matter for `dslide-end', but this ordering matches the situation that would
    ;; occur if the user just called `dslide-forward' repeatedly, and we want the
    ;; end state to be as close to "normal" as possible.
    (mapc #'dslide-end (reverse (oref obj section-actions)))))

(cl-defmethod dslide-final ((obj dslide-slide))
  ;; The order that these are called shouldn't matter.  No use case for coupling
  ;; different finals, but the guarantee is that the lifecycle of the slide
  ;; actions encompass the section actions
  (mapc #'dslide-final (oref obj section-actions))
  (when-let ((slide-action (oref obj slide-action)))
    (dslide-final slide-action))
  ;; Clean up heading marker, which is shared by children
  (set-marker (oref obj begin) nil))

(cl-defmethod dslide-forward ((obj dslide-slide))
  (or (dslide--map-find #'dslide-forward (oref obj section-actions))
      (when-let ((slide-action (oref obj slide-action)))
        (dslide-forward slide-action))))

(cl-defmethod dslide-backward ((obj dslide-slide))
  (or (when-let ((slide-action (oref obj slide-action)))
        (dslide-backward slide-action))
      (dslide--map-find #'dslide-backward (oref obj section-actions))))

(cl-defgeneric dslide--map-find (pred sequence)
  "Find first non-nil return value from mapping PRED over SEQUENCE."
  (let (found)
    (while (and sequence (not found))
      (setq found (funcall pred (pop sequence))))
    found))

;; `dslide--make-slide' is very critical to the user-facing configuration and
;; hacker-facing capabilities and API.  Slides are hydrated from org mode
;; headings.  We can pretty much divide the likely user needs into either what
;; to do with the section and what to do with the child headings.
;;
;; The contents needs to be narrowed to, and this narrowing must be performed
;; both forwards and backwards.  Narrowing and the handling of children are
;; usually coupled, so control over the restriction and the child headings is
;; combined into one slide action.  This action is run outside of section
;; actions, enabling
;;
;; Both slide actions and user configuration have demonstrated a large benefit
;; from being able to slightly change the behavior of actions.  This is why
;; `dslide--make-slide' supports plist arguments when hydrating from org
;; properties and why slide actions that create slides can pass these in via
;; `args'.

(defun dslide--make-slide (heading &rest args)
  "Hydrate a slide object from a HEADING element.
Many optional ARGS.  See code."
  ;; Share the beginning marker across all actions.  It's not unique and
  ;; shouldn't move.
  ;; TODO Consolidate explicit nil indication around whatever is standard
  ;; TODO Haven't needed to specify section actions from the parent yet actions.
  (let* ((begin-position (org-element-property :begin heading))
         (begin (make-marker))
         (inline (plist-get args :inline))
         ;; slide action class can be `none' for explicit nil
         (slide-action-class (plist-get args :slide-action))
         (slide-action-args (plist-get args :slide-action-args)))

    (set-marker begin begin-position (current-buffer))

    ;; Hydrate the slide's configuration as classes and arguments and then
    ;; instantiate them all.
    (let* ((keywords (org-collect-keywords
                      '("DSLIDE_SLIDE_ACTION"
                        "DSLIDE_ACTIONS"
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
           (slide-action (when (and slide-action-class
                                    (not (eq slide-action-class 'none)))
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
            (append (dslide--parse-classes-with-args
                     (or (org-element-property :DSLIDE_ACTIONS heading)
                         (cdr (assoc-string "DSLIDE_ACTIONS" keywords))))
                    dslide-default-actions))
           (section-actions
            (mapcar
             (lambda (c) (and c (apply (if (consp c) (car c) c)
                                  :begin begin
                                  :marker (copy-marker begin)
                                  (append args (cdr-safe c)))))
             section-action-classes))

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
                          :filter filter
                          :begin begin
                          (when (consp class)
                            (cdr class)))))
        slide))))

(cl-defmethod dslide-next-sibling ((obj dslide-slide) filter)
  (when-let* ((heading (dslide-heading obj))
              (next-heading (dslide--next-sibling heading filter)))
    (dslide--make-slide next-heading)))

(cl-defmethod dslide-previous-sibling ((obj dslide-slide) filter)
  (when-let* ((heading (dslide-heading obj))
              (previous-heading (dslide--previous-sibling heading filter)))
    (dslide--make-slide previous-heading)))

(cl-defmethod dslide-heading ((obj dslide-slide))
  "Return the OBJ's heading element."
  (org-element-at-point (oref obj begin)))

;; * Actions
;;; Pre-built Actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Actions are stateful sequences.  They live on a slide.  They usually work on
;; either the section or the children, but there is no requirement that they are
;; exclusive to either.
;;
;; The marker slot, `dslide-marker' method, and `dslide-section-next' and
;; `dslide-section-previous' are of particular utility for "mapping" over
;; elements as the user calls `dslide-deck-forward'.
;;
;; Slide actions should compose with section actions, such
;; as a round-robin slide action cycling through each child's action's forward
;; and backward methods.

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
    :documentation "Draw as if surrounded by other contents.
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

(cl-defgeneric dslide-marker (obj pom)
    "Get or set OBJ's progress marker to optional POM.
Errors when asked for a marker before one has been set.")

(cl-defmethod dslide-marker ((obj dslide-action) &optional pom)
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

(cl-defgeneric dslide-section-next (obj type
                                    &optional pred reverse-in-place
                                      info no-recursion)
  "Return OBJ's next element of TYPE.
Only matches elements beginning after the marker stored in the
action OBJ.  Moves the marker forward to the beginning of the
matched element or to the end of heading.

Optional REVERSE-IN-PLACE is used when changing directions should
return the same element, meaning your action reverses in-place.
hiding and revealing list items works this way.  When non-nil,
matches can include elements starting at the action's marker, and
the marker is moved to the end rather than beginning of an
element.

The action's marker is moved to the end of the heading if no
matching element is found.  This allows a subsequent backwards
step to process the last element.

Optional PRED INFO and NO-RECURSION are the same as for
`dslide-contents-map'.")

(cl-defmethod dslide-section-next ((obj dslide-action) type
                                   &optional pred reverse-in-place
                                     info no-recursion)
  (if-let ((next (dslide--section-next
                  (dslide-heading obj) type (dslide-marker obj)
                  pred reverse-in-place info no-recursion)))
      (prog1 next
        (dslide-marker obj (org-element-property
                            (if reverse-in-place :end :begin) next)))
    (dslide-marker obj (org-element-property :end (dslide-heading obj)))
    nil))

(cl-defgeneric dslide-section-previous (obj type
                                        &optional pred reverse-in-place
                                          info no-recursion)
  "Return previous element of TYPE.
Only matches elements beginning before the marker stored in the
action, OBJ.  Moves the marker backward to the beginning of the
returned element or the beginning of OBJ's heading.

Optional REVERSE-IN-PLACE is used when changing directions should
return the same element, meaning your action reverses in-place.
hiding and revealing list items works this way.  When non-nil,
matches can include elements starting at the action's marker.

Marker is moved to the beginning of the heading if no matching
element is found.  This allows actions to differentiate the begin
state from being at the first matching element.

Optional PRED INFO and NO-RECURSION are the same as for
`dslide-contents-map'.")

(cl-defmethod dslide-section-previous ((obj dslide-action) type
                                       &optional pred reverse-in-place
                                         info no-recursion)
  (if-let ((previous (dslide--section-previous
                      (dslide-heading obj) type (dslide-marker obj)
                      pred reverse-in-place info no-recursion)))
      (prog1 previous
        (dslide-marker obj (org-element-property :begin previous)))
    (dslide-marker obj (org-element-property :begin (dslide-heading obj)))
    nil))

(cl-defgeneric dslide-section-map (obj type fun
                                   &optional info first-match no-recursion)
      "Map FUN over TYPE elements in OBJ's heading's section.
Optional PRED INFO FIRST-MATCH and NO-RECURSION are the same as
for `dslide-contents-map'.")

(cl-defmethod dslide-section-map ((obj dslide-action) type fun
                                  &optional info first-match no-recursion)
  (dslide--section-map
   (dslide-heading obj)
   type fun info first-match no-recursion))

;; begin and end are using the defaults from `dslide-stateful-sequence'.
;; override when extending new actions if inappropriate.

(cl-defmethod dslide-final ((obj dslide-action))
  (when-let ((marker (oref obj marker)))
    (set-marker marker nil)))

;; ** Hide Markup Action
;; TODO allow configuration via plist args
;; TODO precedence order of default actions
(defclass dslide-action-hide-markup (dslide-action) ()
  "Hides element based on type.")

(cl-defmethod dslide-begin ((obj dslide-action-hide-markup))
  (dslide-section-map obj dslide-hide-markup-types
                      (lambda (e) (push (dslide-hide-element e) dslide-overlays)))
  ;; Ooooh! right, yeah, the element parser doesn't give you affiliated keywords
  ;; when you ask for keywords.  As much sense as that would make, the only
  ;; technique I've found for this is falling back to regex.
  (when (member 'keyword dslide-hide-markup-types)
    (save-excursion
      (let ((bound (org-element-property :end (dslide-heading obj))))
        (goto-char (oref obj begin))
        (while (re-search-forward org-keyword-regexp bound t)
          (let ((overlay (make-overlay (match-beginning 0)
                                       (1+ (match-end 0)))))
            (overlay-put overlay 'invisible t)
            (push dslide-overlays overlay)))))))

(cl-defmethod dslide-end ((obj dslide-action-hide-markup))
  (dslide-begin obj))

;; ** Item Reveal Action
;; Reveal items has a somewhat fun implementation.  The end state is actually
;; simpler than the begin state.  Going forward, we must remove overlays and
;; animate items.  Going backward, we add overlays.  When starting at the end,
;; there are no overlays, but when starting at the beginning, all items are
;; concealed by overlays
(defclass dslide-action-item-reveal (dslide-action)
  ((overlays
    :initform nil))
  "Hide all items and then reveal them one by one.")

(cl-defmethod dslide-begin :after ((obj dslide-action-item-reveal))
  (oset obj overlays
        (dslide-section-map
         obj 'item (lambda (e) (dslide-hide-element e (oref obj inline))))))

(cl-defmethod dslide-end ((obj dslide-action-item-reveal))
  (dslide-marker obj (org-element-property :end (dslide-heading obj))))

(cl-defmethod dslide-final :after ((obj dslide-action-item-reveal))
  (mapc #'delete-overlay (oref obj overlays)))

;; TODO Overlay intersection could be consolidated for use in other actions.
(cl-defmethod dslide-forward ((obj dslide-action-item-reveal))
  ;;  Item reveal / hide repeats in place, so we pass a final t to
  ;;  `dslide-section-next'.
  (when-let* ((next-item (dslide-section-next obj 'item nil t))
              (progress (org-element-property :begin next-item)))
    (let ((item-overlays (seq-intersection (oref obj overlays)
                                           (overlays-at (org-element-property
                                                         :begin next-item)))))
      (oset obj overlays (seq-difference (oref obj overlays)
                                         item-overlays))
      (when dslide-slide-in-effect
        (if (oref obj inline)
            (mapc #'dslide-animation-peel item-overlays)
          (dslide-animation-setup (org-element-property :begin next-item)
                                  (org-element-property :end next-item))
          ;; Because the user might add items etc, and to avoid the need for
          ;; keys matching items to our overlays, we intersect overlays we are
          ;; managing with overlays found at point, which could include overlays
          ;; from some other action
          (mapc #'delete-overlay item-overlays))))
    ;; return progress
    progress))

(cl-defmethod dslide-backward ((obj dslide-action-item-reveal))
  (when-let ((previous-item (dslide-section-previous obj 'item))
             (progress (org-element-property :begin previous-item)))
    (push (dslide-hide-element previous-item (oref obj inline))
          (oref obj overlays))
    ;; return progress
    progress))

;; ** Babel Action

;; TODO enable aborting after a failure.  Right now there is a behavior to ask
;; to visit a block
(defclass dslide-action-babel (dslide-action)
  ((remove-results
    :initform t
    :initarg :remove-results
    :description "Remove results when entering slides 🚧.
Experimental.  File an issue if you see something weird.  There's
a lot of behaviors that could potentially react to this option.
Currently only blocks with results set to replace are removed and
only when entering a slide.  Other options seem to suggest
results should not be removed or will never be written to the
buffer anyway."))
  "Execute source blocks as steps.
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


(defun dslide--remove-babel-results (src-block)
  "Remove results if block is configured not to persist them."
  (save-excursion
    (goto-char (org-element-property :begin src-block))
    (let ((args (org-babel-parse-header-arguments
                 (org-element-property :parameters src-block))))
      ;; TODO Add any other values that need results removal.
      (when (member (cdr (assq :results args))
                    '("replace" nil))
        (org-babel-remove-result)))))

(cl-defmethod dslide--clear-all-results ((obj dslide-action-babel))
  (dslide-section-map obj 'src-block
                      #'dslide--remove-babel-results))

(cl-defmethod dslide--hide-non-exports ((obj dslide-action-babel))
  (dslide-section-map
   obj 'src-block
   (lambda (e)
     (save-excursion
       (goto-char (org-element-property :begin e))
       (let ((args (org-babel-parse-header-arguments
                    (org-element-property :parameters e))))
         (when (member (cdr (assq :exports args))
                       '("none" "results"))
           (let ((overlay (dslide-hide-element e)))
             (overlay-put overlay 'dslide-babel-export-control t)
             ;; src-block elements do not appear to contain their results, so it
             ;; seems we do not need to un-hide the results.
             (push overlay dslide-overlays))))))))

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

;; Executing babel seems to widen and also creates messages, and this would
;; result in flashing.  Re-display is inhibited at the deck level to prevent
;; these unpleasantries.  The downside of just inhibiting re-display until after
;; the call is that if re-display is needed, such as if calling `sleep-for' in a
;; loop, then no updates will be visible.  However, the user should really
;; handle this with a timer or process output and process sentinel etc.
(defun dslide--block-execute (block-element)
  (let* ((block-begin (org-element-property :begin block-element))
         (block-end (org-element-property :end block-element))
         (block-marker (copy-marker block-begin))
         (args (org-babel-parse-header-arguments
                (org-element-property :parameters block-element)))
         (export-overlays (seq-filter
                           (lambda (o) (overlay-get o
                                               'dslide-babel-export-control))
                           (overlays-in block-begin block-end))))
    (save-excursion
      (without-restriction
        (goto-char block-begin)
        (condition-case user-wrote-flaky-babel
            ;; t for don't cache.  We likely want effects
            (progn
              (org-babel-execute-src-block t)
              ;; block location could be updated
              (setq block-element (org-element-at-point block-marker))
              (setq block-begin
                    (org-element-property :begin block-element))
              (setq block-end (org-element-property :end block-element))
              (dslide--base-buffer-highlight-region
               block-begin block-end 'dslide-babel-success-highlight)
              ;; updated hiding overlays to not obscure results
              (when (string= (cdr (assq :exports args)) "results")
                (mapc (lambda (overlay)
                        (move-overlay overlay block-begin block-end))
                      export-overlays)))
          ((debug error)
           (dslide--base-buffer-highlight-region
            block-begin block-end 'dslide-babel-error-highlight)
           ;; TODO consolidate moving the point & window points in base buffer
           ;; XXX out of step with other buffer movement
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
  (when (oref obj remove-results)
    (dslide--clear-all-results obj))
  (dslide--hide-non-exports obj)
  (when-let ((block-elements (dslide--get-blocks obj "begin")))
    (mapc #'dslide--block-execute block-elements)))

(cl-defmethod dslide-end ((obj dslide-action-babel))
  ;; Do not use the default implementation because it will play all blocks
  ;; forward.
  (when (oref obj remove-results)
    (dslide--clear-all-results obj))
  (dslide--hide-non-exports obj)
  (dslide-marker obj (org-element-property :end (dslide-heading obj)))
  (when-let ((block-elements (dslide--get-blocks obj "end")))
    (mapc #'dslide--block-execute block-elements)))

(cl-defmethod dslide-final :after ((obj dslide-action-babel))
  (when-let ((block-elements (dslide--get-blocks obj "final")))
    (mapc #'dslide--block-execute block-elements))
  (when (oref obj remove-results)
    (dslide--clear-all-results obj)))

;; ** Image Action

(defclass dslide-action-image (dslide-action)
  ((kill-buffer
    :initform nil
    :initarg :kill-buffer
    :documentation "Kill the buffer.  Default nil just buries it.")
   (include-linked
    :initform nil
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

(cl-defmethod dslide-begin ((obj dslide-action-image))
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
        (when (and (require 'hide-mode-line nil t)
                   (fboundp 'hide-mode-line-mode))
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
        (when (and (require 'hide-mode-line nil t)
                   (fboundp 'hide-mode-line-mode))
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

(cl-defmethod dslide-end ((obj dslide-action-image))
  (dslide-marker obj (org-element-property :end (dslide-heading obj)))
  (dslide-begin obj))

;; * Slide Actions
;; A slide action will generally control the restriction, hydrate children, and
;; pass through `dslide-stateful-sequence' calls to children.  There could be
;; multiple children.  Children that are displayed in the same restriction as
;; the parent will be hydrated with a non-nil `inline' slot value, which tells
;; them not to try to manage the restriction on their own.  This also makes
;; their section actions inline.

(defclass dslide-slide-action (dslide-action)
  ((breadcrumbs
    :initform t
    :initarg :breadcrumbs
    :documentation "Show breadcrumbs in the header.")
   (header
    :initform t
    :initarg :header
    :documentation "Show header."))
  "Base class for slide actions."
  :abstract t)

(cl-defgeneric dslide-narrow (obj &optional with-children)
  "Narrow to OBJ's heading
Optional WITH-CHILDREN will include the child headings in the
restriction.

Inline children have their `inline' slot set to non-nil and will
not attempt to narrow at all.

This function must return nil when it performs no update to the
restriction, meaning no progress was made.")

(cl-defmethod dslide-narrow ((obj dslide-slide-action)
                             &optional with-children)
  (unless (oref obj inline)
    (let* ((heading (dslide-heading obj))
           (begin (oref obj begin))
           (end (if with-children
                    (org-element-property :end heading)
                  (dslide--section-end heading))))
      (unless (and (<= (point-min) begin)
                   (>= (point-max) end))
        (narrow-to-region begin end)
        (when (and dslide-slide-in-effect
                   (not (oref obj inline)))
          (dslide-animation-setup begin end))
        (run-hooks 'dslide-narrow-hook)
        ;; TODO option precedence.  Actions will need to initialize to the
        ;; global value and then refine with explicit per-slide options.
        (when dslide-header
          (let ((dslide-header (oref obj header)))
            (funcall (or dslide-header-fun
                         #'dslide-make-header)
                     nil (oref obj breadcrumbs))))
        (mapc
         (lambda (w) (set-window-point w (point-min))) ; reset the scroll
         (get-buffer-window-list (current-buffer) nil t))
        ;; Return progress
        begin))))

(cl-defgeneric dslide-child-next (obj &optional reverse-in-place)
  "Return the next direct child heading element.
Only matches headings beginning after the marker stored in the
action OBJ.  Moves the marker forward to the beginning of the
matched heading or to the end of heading.

Optional REVERSE-IN-PLACE is used when changing directions should
return the same heading, meaning your action reverses in-place.
Hiding and revealing list items works this way.  When non-nil,
matches can include headings starting at the action's marker, and
the marker is moved to the end rather than beginning of an
heading.

The action's marker is moved to the end of the heading if no
matching heading is found.  This allows a subsequent backwards
step to process the last heading.")

(cl-defmethod dslide-child-next ((obj dslide-slide-action)
                                 &optional reverse-in-place)
  (if-let* ((marker (dslide-marker obj))
            (heading (dslide-heading obj))
            (target-level (1+ (org-element-property :level heading)))
            (next (dslide--contents-map
                   heading 'headline
                   (lambda (child)
                     (and (= target-level (org-element-property :level child))
                          (funcall (if reverse-in-place #'>= #'>)
                                   (org-element-property :begin child) marker)
                          child))
                   nil t)))
      (prog1 next
        (dslide-marker obj (org-element-property
                            (if reverse-in-place :end :begin) next)))
    (dslide-marker obj (org-element-property :end (dslide-heading obj)))
    nil))

(cl-defgeneric dslide-child-previous (obj &optional reverse-in-place)
  "Return the previous direct child heading element.
Only matches child headings beginning before the marker stored in
the action, OBJ.  Moves the marker backward to the beginning of
the returned heading or the beginning of OBJ's heading.

Optional REVERSE-IN-PLACE is used when changing directions should
return the same heading, meaning your action reverses in-place.
hiding and revealing list items works this way.  When non-nil,
matches can include headings starting at the action's marker.

Marker is moved to the beginning of the heading if no matching
heading is found.  This allows actions to differentiate the begin
state from being at the first child heading.")

(cl-defmethod dslide-child-previous ((obj dslide-slide-action)
                                     &optional reverse-in-place)
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
                            (funcall
                             (if reverse-in-place #'<= #'<)
                             (org-element-property :begin child) marker)
                            child)))))))
      (prog1 next
        (dslide-marker obj (org-element-property :begin next)))
    (dslide-marker obj (org-element-property :begin (dslide-heading obj)))
    nil))

;; ** Flat Slide Action
(defclass dslide-slide-action-flat (dslide-slide-action) ()
  "No child slides are created at all.
Narrowing defaults to the entire subtree")

(cl-defmethod dslide-begin ((obj dslide-slide-action-flat))
  (dslide-narrow obj t))

(cl-defmethod dslide-end ((obj dslide-slide-action-flat))
  (dslide-narrow obj t))

;; ** Default Slide Action
(defclass dslide-slide-action-child (dslide-slide-action)
  ((child
    :initform nil
    :documentation "Current child."))
  "Default slide action.
Child headings become independent slides.")

(cl-defmethod dslide-begin ((obj dslide-slide-action-child))
  (dslide-narrow obj))

(cl-defmethod dslide-forward ((obj dslide-slide-action-child))
  ;; For child slides, we make a slide out of the next child heading and advance
  ;; our progress forward to the end of that child
  (or (when-let ((child (oref obj child)))
        (if-let ((progress (dslide-forward child)))
            progress
          (dslide-final child)
          (oset obj child nil)))
      (when-let ((child-heading (dslide-child-next obj)))
        ;; TODO transitive action customization
        (let ((child (dslide--make-slide child-heading)))
          (dslide-begin child)
          (oset obj child child)
          (oref child begin)))))

(cl-defmethod dslide-backward ((obj dslide-slide-action-child))
  ;; For child slides, we make a slide out of the previous child heading and
  ;; advance our progress backward to the beginning of that child.  If there are
  ;; no more children, we narrow back to OBJ's heading
  (or (when-let ((child (oref obj child)))
        (if-let ((progress (dslide-backward child)))
            progress
          (dslide-final child)
          (oset obj child nil)))
      (if-let ((child-heading (dslide-child-previous obj)))
          ;; TODO transitive action customization
          (let ((child (dslide--make-slide child-heading)))
            (dslide-end child)
            (oset obj child child)
            (oref child begin))
        (dslide-narrow obj))))

(cl-defmethod dslide-end ((obj dslide-slide-action-child))
  (dslide-marker obj (org-element-property :end (dslide-heading obj)))
  (let ((point-min (point-min))
        (point-max (point-max)))
    ;; TODO existing hidden babel kind of a hack
    (if-let ((child (dslide-child-previous obj)))
        (let ((child (dslide--make-slide child)))
          (oset obj child child)
          (dslide-end child)
          (when (and (= point-min (point-min))
                     (= point-max (point-max)))
            (dslide-narrow obj)))
      (dslide-narrow obj))))

(cl-defmethod dslide-final :after ((obj dslide-slide-action-child))
  (when-let ((child (oref obj child)))
    (dslide-final child)))

;; ** Inline Slide Action
;; TODO round-robin

(defclass dslide-slide-action-inline (dslide-slide-action)
  ((overlays
    :initform nil)
   (children
    :initform nil
    :documentation "Children that have been instantiated."))
  "Display children inline with the parent.")

(cl-defmethod dslide-begin ((obj dslide-slide-action-inline))
  (dslide-narrow obj t)
  (let ((level (1+ (org-element-property :level (dslide-heading obj)))))
    (oset obj overlays
          (dslide--contents-map
           (dslide-heading obj) 'headline
           (lambda (e)
             (when (= (org-element-property :level e) level)
               (dslide-hide-element e)))))))

(cl-defmethod dslide-forward ((obj dslide-slide-action-inline))
  (let (progress exhausted)
    (while (not (or progress exhausted))
      ;; First try the most recently added child
      (setq progress (when-let* ((child (car (oref obj children))))
                       (dslide-forward child)))
      ;; If the child didn't make progress, try to load up the next child
      (unless progress
        (if-let* ((child-heading (dslide-child-next obj))
                  (child (dslide--make-slide
                          child-heading
                          :slide-action 'dslide-slide-action-inline
                          :inline t)))
            (progn
              (mapc #'delete-overlay
                    (seq-intersection (oref obj overlays)
                                      (overlays-at (org-element-property
                                                    :begin
                                                    child-heading))))
              (dslide-begin child)
              (when dslide-slide-in-effect
                (dslide-animation-setup
                 (org-element-property :begin child-heading)
                 (org-element-property :end child-heading)))
              (setq progress child)
              (push child (oref obj children)))
          (setq exhausted t))))
    progress))

(cl-defmethod dslide-backward ((obj dslide-slide-action-inline))
  (let (progress)
    (while (and (oref obj children) (not progress))
      ;; First try the most recently added child
      (setq progress (when-let* ((child (car (oref obj children))))
                       (dslide-backward child)))

      ;; If the child didn't make progress, hide it with an overlay
      (unless progress
        (let* ((finished (pop (oref obj children)))
               (heading (dslide-heading finished)))
          (dslide-child-previous obj) ; for marker effects 💡
          (push (dslide-hide-element heading) (oref obj overlays))
          (dslide-final finished)
          (setq progress (or (car (oref obj children))
                             (dslide-heading obj))))))
    progress))

(cl-defmethod dslide-end ((obj dslide-slide-action-inline))
  (dslide-narrow obj t)
  (dslide-marker obj (org-element-property :begin (dslide-heading obj)))
  (let (exhausted)
    (while (not exhausted)
      ;; If the child didn't make progress, try to load up the next child
      (if-let* ((child-heading (dslide-child-next obj)))
          (let* ((child (dslide--make-slide
                         child-heading
                         :slide-action 'dslide-slide-action-inline
                         :inline t)))
            (let ((dslide-slide-in-effect nil))
              (dslide-end child))
            (push child (oref obj children)))
        (setq exhausted t)))))

(cl-defmethod dslide-final :after ((obj dslide-slide-action-inline))
  (mapc #'delete-overlay (oref obj overlays))
  (mapc #'dslide-final (oref obj children)))

;; ** Every Child Action
(defclass dslide-slide-action-every-child (dslide-slide-action)
  ((overlay
    :initform nil)
   (children
    :initform nil
    :documentation "Children that have been instantiated."))
  "Display children inline and step every child at once.")

(cl-defmethod dslide-begin ((obj dslide-slide-action-every-child))
  (dslide-narrow obj t)
  (oset obj overlay (dslide-hide-children (dslide-heading obj))))

;; TODO multi-progress, like babel blocks
(cl-defmethod dslide-forward ((obj dslide-slide-action-every-child))
  (let ((progress))
    (while-let ((child-heading (dslide-child-next obj))
                (child (dslide--make-slide
                        child-heading
                        :slide-action 'dslide-slide-action-every-child
                        :inline t)))
      (dslide-begin child)
      ;; TODO inline animate in
      (setq progress child)
      (push child (oref obj children)))
    (when progress
      (delete-overlay (oref obj overlay))
      (oset obj overlay nil))
    (unless progress
      (let ((progresses (mapcar #'dslide-forward
                                (oref obj children))))
        (setq progress (seq-first progresses))))
    progress))

(cl-defmethod dslide-backward ((obj dslide-slide-action-every-child))
  (let (progress)
    (let ((progresses (mapcar #'dslide-backward
                              (oref obj children))))
      (setq progress (seq-first progresses)))
    (unless progress
      (oset obj overlay (dslide-hide-children (dslide-heading obj)))
      (mapc #'dslide-final (oref obj children))
      (oset obj children nil)
      ;; TODO just noticed that the marker method could store a marker that is
      ;; shared, which could result in actions clobbering each other's sense of
      ;; progress. ☢️
      (set-marker (oref obj marker) nil)
      (dslide-marker obj (copy-marker (oref obj begin))))
    progress))

(cl-defmethod dslide-end ((obj dslide-slide-action-every-child))
  (dslide-narrow obj t)
  (while-let ((child-heading (dslide-child-next obj)))
    (push (dslide--make-slide
           child-heading
           :slide-action 'dslide-slide-action-every-child
           :inline t)
          (oref obj children)))
  (mapc #'dslide-end (oref obj children)))

(cl-defmethod dslide-final :after ((obj dslide-slide-action-every-child))
  (when-let ((overlay (oref obj overlay)))
    (delete-overlay overlay))
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
    (heading type after &optional pred inclusive info no-recursion)
  "Return HEADING's next element of TYPE.
By default, matches only after AFTER but with optional INCLUSIVE,
also includes matches at AFTER.

PRED, INFO, FIRST-MATCH, and NO-RECURSION are described in
`org-element-map'."
  (let* ((combined-pred
          (dslide-and pred
                      (lambda (e) (funcall (if inclusive  #'>=  #'>)
                                      (org-element-property :begin e) after)))))
    (dslide--section-map
     heading type combined-pred info t no-recursion)))

(defun dslide--section-previous
    (heading type before &optional pred inclusive info no-recursion)
  "Return HEADING's previous element of TYPE.
By default, matches only before BEFORE but with optional
INCLUSIVE, also includes matches at BEFORE.

PRED, INFO, FIRST-MATCH, and NO-RECURSION are described in
`org-element-map'."
  (let* ((combined-pred (dslide-and
                         pred
                         (lambda (e) (funcall (if inclusive #'<= #'<)
                                         (org-element-property :begin e) before)))))
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
       (and (or (not pred)
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

(defun dslide-make-header (cleanup &optional breadcrumbs)
  "Draw a header for the first tree in the restriction.
CLEANUP is non-nil if we are only cleaning up state.

Set optional BREADCRUMBS to non-nil to create breadcrumbs.  The
implementation assumes the buffer is restricted and that there is
a first tree.  You may use `dslide-overlays' to benefit
from existing state cleanup."
  (when dslide--header-overlay
        (delete-overlay dslide--header-overlay))

  (unless cleanup
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
                   (propertize title 'face '(org-document-title default))
                   (dslide--margin-lines dslide-margin-title-below)
                   (when (and  dslide-header-date date)
                     (dslide--info-face (concat date "  ")))
                   (when (and  dslide-header-author author)
                     (dslide--info-face (concat author "  ")))
                   (when (and  dslide-header-email email)
                     (dslide--info-face (concat email "  ")))
                   (when (and breadcrumbs
                              dslide-breadcrumb-separator)
                     (concat (dslide--info-face "\n")
                             (dslide--get-parents
                              dslide-breadcrumb-separator)))
                   (dslide--margin-lines dslide-margin-content)))

        (overlay-put dslide--header-overlay 'before-string
                     (dslide--margin-lines dslide-margin-content))))))

(defun dslide--info-face (s)
  (propertize s 'face '(org-document-info default)))

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

;; * Animation

(defun dslide-animation-peel (overlay)
  "Peel away and delete OVERLAY."
  (let ((timer (timer-create))
        (peel-rate (/ dslide-animation-duration
                      (max 1 (- (overlay-end overlay)
                                (overlay-start overlay))))))
    (push timer dslide--animation-timers)
    (push overlay dslide--animation-overlays)
    (timer-set-time timer (current-time) peel-rate)
    (timer-set-function timer #'dslide--animate-peel
                        (list timer overlay))
    (timer-activate timer)))

;; TODO move respect for animation variables into this function.  Create a
;; sensible integration between sliding and peeling animation.  Add a customize interface.
;; TODO Support non-graphical
;; TODO User-provided animation override function
(defun dslide-animation-setup (beg end)
  "Slide in the region from BEG to END.
Everything after BEG will be animated.  The region between BEG
and the value of `point-max' should contain a newline somewhere."
  (dslide--ensure-slide-buffer)
  (without-restriction
    (let* ((buffer-invisibility-spec nil)
           (timer (timer-create))
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
      (overlay-put overlay 'line-height (float dslide-slide-in-blank-lines))
      (overlay-put overlay 'priority 10)
      (push timer dslide--animation-timers)
      (push overlay dslide--animation-overlays)
      (timer-set-time timer (current-time)
                      dslide-animation-frame-duration)
      (timer-set-function timer #'dslide--animate
                          (list timer goal-time overlay initial-line-height))
      (timer-activate timer))))

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
           (line-height (+ initial-line-height (* lines fraction))))
      (overlay-put overlay 'line-height line-height))))

(defun dslide--animate-peel (timer overlay)
  (let ((start (overlay-start overlay))
        (end (overlay-end overlay)))
    (move-overlay overlay (1+ start) end)
    (when (= (1+ start) end)
      (cancel-timer timer)
      (setq dslide--animation-timers
            (delq timer dslide--animation-timers))
      ;; TODO evaporation?
      (delete-overlay overlay)
      (setq dslide--animation-overlays
            (delq overlay dslide--animation-overlays)))))

;; If you want to make a custom animation, just make sure that it is cleaned up
;; by this function.  Timers go in `dslide--animation-timers'.  Overlays go in
;; `dslide--animation-overlays'.
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
                 (buffer-substring-no-properties
                  headline-begin (1- headline-end)))))))

(defun dslide--cleanup-state ()
  "Clean up states between contents and slides."
  (when dslide-header
    (funcall (or dslide-header-fun
                 #'dslide-make-header)
             t nil))
  (dslide--delete-overlays)
  (dslide--animation-cleanup)
  ;; TODO oref & oset outside of class
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
  (while dslide-overlays
    (delete-overlay (pop dslide-overlays)))
  (while dslide--step-overlays
    (delete-overlay (pop dslide--step-overlays)))
  (when dslide--contents-hl-line-overlay
    (delete-overlay dslide--contents-hl-line-overlay))
  (setq dslide--contents-hl-line-overlay nil))

(defun dslide--ensure-slide-buffer (&optional display)
  "Switch to the slide buffer.
Use this in functions which must run in the slide buffer but
could be called from another buffer. Optional DISPLAY will also
ensure that the slide buffer is visible."
  (unless (dslide-live-p)
    (error "Live deck not found"))
  (if display
      (let ((slide-buffer (oref dslide--deck slide-buffer)))
        ;; TODO requires further taming if user is using multiple frames, such
        ;; as displaying some information on a projector and other information
        ;; on another frame
        (unless (get-buffer-window slide-buffer)
          (display-buffer slide-buffer dslide--display-actions)))
    (set-buffer (oref dslide--deck slide-buffer))))

(defun dslide--keyword-value (key)
  "Get values like #+KEY from document keywords."
  (cadr (assoc-string key (org-collect-keywords (list key)))))

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
  (when-let ((symbol (or (and (symbolp filter-name) filter-name)
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
  :interactive nil
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

(defun dslide-display-slides ()
  (dslide--ensure-slide-buffer t)
  (dslide--cleanup-state)
  (oset dslide--deck slide-buffer-state 'slides)
  (widen)
  (org-fold-show-all)
  (dslide-begin dslide--deck))

(defun dslide-display-develop ()
  (let ((slide-buffer (oref dslide--deck slide-buffer)))
    (unless (and (get-buffer-window slide-buffer)
                 (dslide--showing-slides-p))
      (dslide-display-slides)))
  (let ((base-buffer (oref dslide--deck base-buffer)))
    (unless (get-buffer-window base-buffer)
      (display-buffer base-buffer 'display-buffer-pop-up-window))))

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

;; TODO use this to implement `dslide-goto'
(defun dslide--follow (progress &optional scroll)
  "Set the base buffer window point to PROGRESS.
PROGRESS is a slide object, marker, buffer position, org element,
org object or boolean (which will be ignored).  Optional scroll
will advance the windows to the current buffer restriction"
  (unless (dslide-live-p)
    (error "Live deck not found"))
  (let ((pos (cond ((integerp progress) progress)
                   ((eieio-object-p progress)
                    (marker-position (oref progress begin)))
                   ((markerp progress) (marker-position progress))
                   ((and (listp progress)
                         (or (member (car progress) org-element-all-elements)
                             (member (car progress) org-element-all-objects)))
                    (org-element-property :begin progress))))
        slide-buffer-point-min)
    (when (and (not (booleanp progress)) (null pos))
      (message "Incomprehensible progress reported: %s" progress))
    (when (and pos dslide-base-follows-slide)
      (set-buffer (oref dslide--deck slide-buffer))
      (setq slide-buffer-point-min (point-min))
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
      (when-let ((windows (get-buffer-window-list (current-buffer) nil t)))
        (mapc (lambda (w)
                (set-window-point w pos)
                (when scroll
                  (with-selected-window w
                    (goto-char slide-buffer-point-min)
                    ;; TODO 10 is completely arbitrary
                    (recenter 10))))
              windows))
      (set-buffer (oref dslide--deck slide-buffer)))))

(defun dslide-display-contents ()
  "Switch to showing contents in the slide buffer.
This is a valid `dslide-start-function' and will start
each slide show from the contents view."
  (dslide--ensure-slide-buffer t)
  (dslide-final dslide--deck)
  (dslide--cleanup-state)
  (oset dslide--deck slide-buffer-state 'contents)
  (widen)
  (org-overview)
  (goto-char (org-element-property :begin (dslide--root-heading-at-point)))
  (recenter)

  (if-let ((first (dslide--document-first-heading)))
      (narrow-to-region (org-element-property :begin first)
                        (point-max))
    ;; No first heading.  Just header.  Empty contents.
    (narrow-to-region (point-max)
                      (point-max)))
  (run-hooks 'dslide-narrow-hook)

  (when dslide-header
    (funcall (or dslide-header-fun #'dslide-make-header) nil nil))

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
      (if (get-buffer-window (oref dslide--deck slide-buffer))
          (progn
            (dslide--ensure-slide-buffer)
            (if (dslide--showing-slides-p)
                ;; TODO check for secondary task here
                (dslide-display-contents)
              (dslide--choose-slide dslide--deck 'contents)
              (dslide-display-slides)))
        (dslide--ensure-slide-buffer t))
    (let ((dslide-start-function #'dslide-display-slides))
      (dslide-mode 1))))

;;;###autoload
(defun dslide-deck-develop ()
  "Show both the base and slide buffer."
  (interactive)
  (let ((major-mode (buffer-local-value 'major-mode (current-buffer))))
    (unless (or (dslide-live-p) (derived-mode-p 'org-mode))
      (user-error "Not an org buffer and no other live presentation"))
    (if (dslide-live-p)
        ;;  show the correct buffers
        (dslide-display-develop)
      (let ((dslide-start-function #'dslide-display-develop))
        (dslide-mode 1)))))

;;;###autoload
(defun dslide-deck-forward ()
  "Advance slideshow forward."
  (interactive)
  ;; TODO this does not display the buffer because we don't have any buffer
  ;; tracking yet.  Babel integrations with other buffers must take care, as
  ;; must presenters.
  (dslide--ensure-slide-buffer)
  (if (dslide--showing-contents-p)
      (progn (org-next-visible-heading 1)
             (while dslide--step-overlays
               (delete-overlay (pop dslide--step-overlays)))
             (dslide--follow (point)))
    (dslide--ensure-slide-buffer)
    (dslide-forward dslide--deck)))

;;;###autoload
(defun dslide-deck-backward ()
  "Advance slideshow backward."
  (interactive)
  ;; TODO this does not display the buffer because we don't have any buffer
  ;; tracking yet.  Babel integrations with other buffers must take care, as
  ;; must presenters.
  (dslide--ensure-slide-buffer)
  (if (dslide--showing-contents-p)
      (progn (org-previous-visible-heading 1)
             (while dslide--step-overlays
               (delete-overlay (pop dslide--step-overlays)))
             (dslide--follow (point)))
    (dslide--ensure-slide-buffer)
    (dslide-backward dslide--deck)))

(provide 'dslide)

;;; dslide.el ends here

;; Local Variables:
;; outline-regexp: ";; \\(*+\\)"
;; End:
