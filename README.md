
https://github.com/positron-solutions/dslide/assets/73710933/06a66e42-a172-48ba-968f-5f5b1989a868

# A Presentation Framework

-   Present Org documents with babel integration
-   Incorporate **anything** Emacs does into a presentation
-   Extensible for org and beyond


## Simple User Interface

Fully programmable sequences behind a two-button interface:

-   `dslide-deck-forward`
-   `dslide-deck-backward`


## Present Org Documents 🦄

-   Document header generated from keywords
-   Breadcrumbs
-   By default, every heading and child heading is a slide
-   Configurable slide behavior using pre-built actions
-   Consume typical org data like inline images with full-frame display


## Fully Programmable ✨

-   Directly script your presentation with Org babel blocks
-   Convenient API for quickly writing reliable custom actions
-   Integration with Elisp programs and arbitrary Emacs buffers
-   Custom class support for extending the framework


## Status 🛠️

Version 0.5.2 👷

-   Stabilizing the API and naming (as in **not completely** stable yet)
-   Gathering user feedback & experience to pinpoint actual use patterns
-   Accepting PR's and issue reports
-   Some behaviors may be advertised as working already when they are only 90% implemented.  **File issues**.

The user-facing configuration API has been pretty unstable, but now that nested slide actions are supported, it is likely to remain roughly like it is now.


# Installation

This isn't on a package archive yet.  Subscribe to Positron's [YouTube](https://www.youtube.com/@Positron-gv7do) for updates.

    ;; package-vc
    (package-vc-install
     '(dslide
       :url "https://github.com/positron-solutions/dslide.git"))
    
    ;; using elpaca's with explicit recipe
    (use-package dslide
      :elpaca (dslide :host github
                            :repo "positron-solutions/dslide"))
    
    ;; straight with explicit recipe
    (use-package dslide
      :straight (dslide :type git :host github
                              :repo "positron-solutions/dslide"))
    
    ;; or use manual load-path & require, you brave yak shaver


## Try It Out

With just defaults, run `dslide-deck-start` on your existing documents.  You can load the examples in the <./test/demo.md> file to see a showcase of configuration behavior.

The default keymap uses arrow keys.  Left and right are `dslide-deck-forward` and `dslide-deck-backward`.  Up is `dslide-deck-start` and will show the contents.  Down is `dslide-deck-stop` and will stop the slide show.


# Features


## Contents Navigation

Call `dslide-contents` to show a contents overview.  Calling `dslide-deck-forward` and `dslide-deck-backward` in the contents can quickly move through headings.  Call `dslide-deck-start` again to resume the presentation from that point.


## Clean Buffer State

The actual display is done in an indirect buffer.  Your hooks and customizations for presentation will not pollute your editing buffer.  Dirty state will not pile up in your presentation buffer, greatly increasing reliability even if your custom Elisp scripting is sloppy 💩.


## Follow Along

Check out `dslide-deck-develop`.  You can see the markup and the returned approximate progress indications.  Babel actions will highlight blocks as they execute.


## Hides Markup

By default, the `dslide-action-hide-markup` action is configured in `dslide-default-actions`.  Looks clean out of the box.

🚧 The current element hiding is implemented with overlays.  I can be done with font-locking, but this is easier done in a minor mode like how `org-modern` works.


# Glossary

-   **Deck**: an object that is used to relate the display and base buffer and is the root of all sequences.  It's another word for "presentation" or PT.
-   **Slide**: an object that interprets an org heading to hydrate its actions
-   **Action**: an object that responds to `dslide-deck-forward` and `dslide-deck-backward` calls and implements lifecycle methods to initialize and clean up state
    -   **Slide Action**: an action subclass that handles initial display of the slide and creation of child slides from sub-headings.
    -   **Section Actions**: actions that typically control the display and behavior of a heading's section, the region between the headline and child headings.
-   **Step**: a single call to `dslide-deck-foward` or `dslide-deck-backward`, usually delegated down to `dslide-forward` and `dslide-backward` methods
-   **Contents**: A view of the folded root headings that is used for quickly navigating between headings during a presentation.
-   **Slide Buffer**: the slides are shown in an indirect buffer that is cloned from your org document buffer.  The source is called the **base buffer**.  Check for the `deck: my-presentation.org` buffer name.
    -   Indirect buffer and `slide-buffer` are used interchangeably
    -   Base buffer or `base-buffer` is used pretty exclusively


# Configuring

Be sure to check `M-x` `customize-group` `dslide` to see all declared custom variables. All of the variables are configured to recommended defaults except hooks, which would depend on other packages usually.

Many settings can be configured at:

-   global level through customize variables
-   document level through keywords
-   slide level through the property drawer


## Binding

You likely want to start the mode via `dslide-deck-start`.  Once the mode starts, it creates an indirect buffer to display the slides and then calls `dslide-deck-start-function` once the mode is active and everything is initialized, so you can customize startup behavior.

All commands begin with `dslide-deck` 💡

    (keymap-set org-mode-map "<f5>" #'dslide-deck-start)

Once the global minor mode, `dslide-mode` is active, additional bindings in `dslide-mode-map` are active in every buffer so that you can integrate other buffers into your presentation.  (Tracking which buffers are part of a presentation is  still a topic under consideration 🚧)


### Secondary Commands 🚧

Because you might want to play a video or take a branch in the presentation and then exit that branch, the plan is to overload the `dslide-deck-start` binding within presentations to enter / exit these branches.


## Hooks

Because slides and actions have a life-cycle and can easily find their own heading, consider making a custom action and setting that action on slides where it's needed.

Beware of using the normal `dslide-mode-hook` 😱 because it runs **in the base buffer** ⚠️.  If you remap faces or add a bunch of styling, it will be copied to the indirect buffer but then linger in your base buffer.  Instead, use `dslide-start-hook`. 💡

-   `dslide-start-hook` Is run in the indirect buffer after it is set it.  This is what you want.
-   `dslide-stop-hook` is run in the base buffer because the indirect buffer is already dead.
-   `dslide-contents-hook` is run after switching to contents.  It runs in the display buffer.
-   `dslide-narrow-hook` is run whenever a `dslide-deck-forward` or `dslide-deck-backward` changes the narrow state.
-   `dslide-after-last-slide-hook` is run when the user tries to go forward but there are no more slides.  You can use this to implement a final feedback before quitting or add `dslide-deck-stop` to exit without feedback.
    
    Another option is to use `dslide-push-step` to push a callback that will only run when called going forward.

    (defun my-stop-if-forward ()
      (mc-push-step (lambda (direction)
                      (when (eq direction 'forward)
                        ;; Be sure to return t or the hook will run again.
                        (prog1 t (dslide-deck-stop))))))
    
    (setq dslide-after-last-slide-hook #'my-stop-if-forward)


## Heading Properties

Headings are treated as slides.  Slides have actions.  Actions are configured in the property drawer.

-   `DSLIDE_SLIDE_ACTION`: Usually narrows to the slide and creates children from child headings.  Lifecycle encloses the section.
-   `DSLIDE_ACTIONS:` Most commonly customized.  You can list multiple actions.  Each one will step through its forward and backward steps.

Some actions must be fully enclosed by the lifecycle of a surrounding action, such as narrowing to the headline and section before displaying a contained list item-by-item.

🚧 Likely in the future, actions will be composable and accept arguments, using Lisp s-expressions.  This API should be forward compatible.


### Example

Regular Org Mode markup is used to add actions to headings.  See more examples in the <../test> directory.

    * Full Screen Images
    :PROPERTIES:
    :DSLIDE_ACTIONS: dslide-action-images
    :END:
    #+attr_html: :width 50%
    [[./images/emacsen4.jpeg]] [[./images/before-google3.jpeg]]


### Action Arguments

Many actions understand arguments, allowing tuning of similar behaviors from the same class.  Implementing new arguments is relatively easy, just adding a slot and then reacting to the value of that slot.

Configuring the slot is done by adding plist-style properties after the class name:

    :PROPERTIES:
    :DSLIDE_ACTIONS: dslide-action-item-reveal :inline t
    :END:

You can also use "property+" syntax to add to a property, and these accept plist arguments too:

    :PROPERTIES:
    :DSLIDE_ACTIONS: dslide-action-babel
    :DSLIDE_ACTIONS+: dslide-action-images :fullscreen t
    :END:


# Customizing


## Sub-classing

The deck and slide class as well as actions can all be sub-classed.  Use the existing sub-classes of actions as example code for writing other classes.  See the [eieio#Top](info:eieio#Top) manual for explanation of OOP in Elisp.

-   **Action**:  Creating new action subclasses are an efficient way to perform similar operations on typical kinds of org data.
-   **Slide:**  Slides can be configured extensively by changing their actions.  However, for more vertical cooperation between slides or cooperation among actions, extended slides could be useful.
-   **Deck**:  If the core methods of the deck are insufficient, extension is another option besides advice, hooks, and modifying the source.

If you suspect you might need to sub-class the `dslide-slide` or `dslide-deck`, please file an issue because your use case is probably interesting.


### Custom Action

The `dslide-section-next`  and `dslide-section-previous` method documentation are very helpful behavior for quickly writing custom actions.  They advance the action's `:marker` forwards and backwards to the next matching element and return that element so we can do something with it.

-   declare a class
-   override a few methods
-   now you too can paint the paragraphs red

You can view the effect of this example in the demo.

    (defclass dslide-action-red-paragraphs (dslide-action)
      ((overlays :initform nil))
      "Paint the paragraphs red, one by one.")
    
    ;; Default no-op `dslide-begin' is sufficient
    
    ;; Default implementation of `dslide-end', which just plays forward to the end,
    ;; is well-behaved with this class.
    
    ;; Remove any remaining overlays when calling final.
    (cl-defmethod dslide-final :after ((obj dslide-action-red-paragraphs))
      (mapc #'delete-overlay (oref obj overlays)))
    
    ;; Find the next paragraph and add an overlay if it exists
    (cl-defmethod dslide-forward ((obj dslide-action-red-paragraphs))
      (when-let ((paragraph (dslide-section-next obj 'paragraph)))
        (let* ((beg (org-element-property :begin paragraph))
               (end (org-element-property :end paragraph))
               (new-overlay (make-overlay beg end)))
          (overlay-put new-overlay 'face 'error)
          (push new-overlay (oref obj overlays))
          ;; Return non-nil to indicate progress was made.  This also informs the
          ;; highlight when following the slides in the base buffer.
          beg)))
    
    (cl-defmethod dslide-backward ((obj dslide-action-red-paragraphs))
      (when-let* ((overlay (pop (oref obj overlays))))
        (delete-overlay overlay)
        ;; If there is a preceding overlay, move to its beginning else move to the
        ;; beginning of the heading.
        (if-let ((overlay (car (oref obj overlays))))
            (dslide-marker obj (overlay-start overlay))
          (dslide-marker obj (org-element-property :begin (dslide-heading obj))))))


## Default Classes

The default classes and actions can be configured at the document or customize level.  Set the `DSLIDE_DECK_CLASS` and `DSLIDE_SLIDE_CLASS` as well as other properties that work at the heading level.  The order of precedence (**Not fully implemented** 🚧):

-   Property definition of the current heading
-   Property definition in the document
-   Customize variable


## Babel Scripting

You can write custom scripts into your presentation as Org Babel blocks.  These can be executed with the `dslide-action-babel` action.  You just need to label your blocks with lifecycle methods if you want to be able to go forwards and backwards.  See the `dslide-action-babel` class and examples in <./test/demo.md>.

The `#+attr_dslide:` affiliated keyword is used to configure which methods will run the block.  Block labels that are understood:

-   `begin` and `end` are run when the slide is instantiated, going forward and backward respectively.  You can have several blocks with these methods, and they will be run from **top-to-bottom** always, making it easier to re-use code usually.

-   `final` is only called when no progress can be made or if the presentation is stopped.

-   `forward` and `backward` are self-explanatory.  Position your `backward` blocks **above** any block that they undo

-   `both` runs either direction.  It will not repeat in place when reversing.  Use seperate `forward` and `backward` blocks for that 💡


### Step Callbacks

See `dslide-push-step` for inserting arbitrary callbacks that can function as steps.  Unless your action performs state tracking to decide when to consume `dslide-deck-forward` and `dslide-deck-backward` itself, a callback may be easier.

Because babel blocks are not actions, using `dslide-push-step` may be the only way to optionally add a step callback from a babel block.


# Package Pairings

This package is focused on creating a linear presentation sequence. For functionality not related to integrations into the `dslide-deck-forward` `dslide-deck-backward` interface, it is better to maintain separate packages and use hooks and babel scripting.


## Master of Ceremonies

The [master-of-ceremonies](https://github.com/positron-solutions/master-of-ceremonies) package contains utilities for display & presentation frame setup that are not specific to using DSL IDE.

-   display a region full-screen using `mc-focus`.  Check the full commands by pressing `h` during focus.  You can highlight a region, save an expression to playback a code snippet without the buffer open etc.
-   silence messages during presentation
-   hide the cursor or make it very subtle

    ;; Also check `mc-subtle-cursor-mode'
    (add-hook 'dslide-start-hook mc-hide-cursor-mode)


## Open Broadcaster Software

Sacha Chua has written an OBS plugin integration helpful for video integration [obs-websocket-el](https://github.com/sachac/obs-websocket-el).


## Orgit

`orgit` can be used to show commits as links, which open with `dslide-action-links` 🚧  This is a lie.  I was going to support this as a demonstration of a custom action.


## moom.el

The [moom](https://github.com/takaxp/moom#org-mode-org-tree-slide) package contains some commands for resizing text and repositioning frames.


## Org Modern

Bullets and many prettifications of common org markups.  The markup that you don't hide looks better with org modern.


## Org Appear

Never worry about turning on pretty links for a presentation.  Edit them by just moving the point inside.


# Domain Model

This is a description of how the pieces of the program **must** fit together.  For any deep customization or hacking, the section is essential reading.  At the least, it will **greatly improve your success**.

⚠️ <del>Even if the current implementation differs, trust this domain model and expect the implementation to approach it.</del>  **This section is pretty accurate as of 0.5.0**

-   The user interface `dslide-deck-forward` and `dslide-deck-backward` is a concrete requirement that drives most of the rest of the implementation and feature design.
-   Because Org's basic structure is about trees, we need to nest these sequences.  Flattening the tree was more limiting and not chosen.
-   There is a little bit of action composition because the slide action always runs outside the life cycle of the other actions.  This allows it to control the buffer restriction or switch to a child in the appropriate order.


## Stateful Sequence Class

This class is the heart of providing the common user interface and convenient implementation interface for extending the package.


### Command Pattern

The basis of all undo systems is either:

-   implement reverse actions that decide their behavior from the updated state
-   save mementos that allow undoing forward actions.

This is the [command pattern](https://en.wikipedia.org/wiki/Command_pattern).  Navigating the linear sequence of a presentation is very similar to an undo system.  Log-backed architectures such as git or event-sourcing can similarly be viewed as navigating to any point in a sequence by applying or rolling back a sequence of changes.


### Setup & Teardown

At the boundaries of a sequence of forward and reverse actions, it may be necessary to build up or tear down some state.

There are two setup methods:

-   `dslide-begin` for setup going forwards
-   `dslide-end` for setup going backwards

Additionally, for teardown there is `dslide-final` that is always called last, when the action or slide will be garbage collected and wants to clean up overlays etc.


### Indexing Via Point

In order to support contents based navigation, we need to be able to play a slide forward up to the current point.  This may require instantiating some parent slides and playing them forward to a child.  To avoid the need for parents to know about children, the `dslide-goto` method was introduced.


### Stateful Sequence Interface

The conclusion of the command pattern, setup & teardown, and indexing via point is the `dslide-stateful-sequence` class.  Anything that implements its interface can be controlled by `dslide-deck-forward` and `dslide-deck-backward`.  The full interface:

-   `dslide-begin` & `dslide-end`
-   `dslide-final`
-   `dslide-forward` & `dslide-backward`
-   `dslide-goto`

-   Re-Using Implementations

    -   The default implementation of `dslide-end` is achieved by just walking forward from `dslide-begin`, calling `dslide-forward` until it returns `nil`.
    
    -   Implementing `dslide-goto` is optional as long as `dslide-begin` and `dslide-forward` can implement `dslide-end` and report their furthest extent of progress accurately.
    
    -   Ideally `dslide-deck-forward` & `dslide-deck-backward` along with `dslide-begin` & `dslide-end` form a closed system, but for the convenience of the implementer, it's fine to use an idempotent `dslide-begin` as the `dslide-deck-backward` step if granular backward is difficult or not valuable to implement.


## Sequence Composition

Navigating a tree involves depth.  Descendants may care about what happened in ancestors.  Ancestors may care about what descendants leave behind.  There may be conventions about what happens when descending into a child or returning from one.


### Telescoping Calls

At one time, slides were to be mostly independent and not running at the same time.  While this simplified some things, it was limited.

Nesting slides and calling their actions might require updating several children concurrently.  This was impossible to implement without pulling logic down into the parent slide's actions.  Thus, the implementation calls through parents into children, sometimes calling several children.


### Slide Actions

The reason slide actions are distinct from other actions:

1.  They need to encompass the lifecycle of the "section" actions
2.  Narrowing and handling the display of inline child slides are a coupled problem.

The lifetime of the slide action encompasses the section actions.  It narrows or switches to a childe before the section actions attempt to work on the contents.


### Trees & Lifetime

If something depends on something else existing or having been set up, its lifetime must be fully encompassed by that other thing.  Especially since we are going forward & backward, setup & cleanup must happen on both ends of a sequence.

It is natural that a parent heading out-lives its child.  User can take advantage of this by using the document or higher level headings to store state that needs to be shared by children.  The `final` calls for those things can call cleanup.


### Slides & Action Lifetime

Actions live, for the most part, as long as the slide.  Their `dslide-begin` method is called at the very beginning.  An action that reveals items must hide them before the user first sees them.

A consequence of this is that there are usually multiple actions alive at once.  Something has to hold onto them.  This is the slide.


# Contributing

-   Since you likely just need something to magically happen, the recommended option is to place a hamburger in the [hamburger jar](https://github.com/sponsors/positron-solutions) and file an issue.
-   If you do have time, excellent.  Happy to support your PR's and provide context about the architecture and behavior.


## Work In Progress 🚧

Open issues and give feedback on feature requests.  Contributions welcome.


### Slide Action Precedence

When a slide is created in `dslide-make-slide`, it can obtain them from several places:

-   passed in arguments, as slide actions do to prevent children from trying to display themselves
-   properties, how slides are usually configured
-   customize variables that set the default actions.

The order of precedence and capability to override options is still pretty immature.


### Secondary Commands

See the section about bindings for context.  Video play or other situations where the presentation might branch should be supported by overloading the behavior of `dslide-deck-start`.  I think this command will turn into `dslide-deck-secondary` in the `dslide-mode-map`.


### `dslide-goto`, starting from point

Since not many actions currently have implemented this very accurately, playing from point is likely not that accurate.  Progress updating in the base buffer is also currently only at the slide level of granularity.


### Affiliated Buffers

There is no tracking whether a buffer is part of the presentation or not.  How would a buffer become one?  Should it be implicit?  Without any sort of tracking, the consequence is that having a presentation open leaves the minor mode bindings hot.  These commands do weird things when run from these situations, especially if running babel scripts, so some kind of first-class buffer affiliation seems necessary.


### Non-Graphic Display

For terminals, the line-height based slide-in effect is not supported.


### Sub-Sequence Call & Restore

Sequences are often enclosed within other sequences, but there is currently no support for pushing or popping states when entering or exiting sequences.  It's just not clear yet what cooperation might be necessary at sub-sequence boundaries.


### Non-Org Sequences

There's no concrete reason why presentations need to start with Org mode buffers.  The deck object could have its org-specific functionality pushed down to an org-mode class.  The only requirement is to be able to hydrate some stateful sequences, which may hydrate and call into sub-sequences, meaning anything is pretty trivially possible.


### Heading Filtering

This was not implemented yet, but evidently some had been filtering their headlines to only show TODO's in `org-tree-slide`.  Perhaps it is convenient to filter some tags and prevent them from being instantiated, especially if they will fail.


### Counting Slides

Especially if slides launch sub-sequences, and they do it from Lisp, this is hard.  Buffer slides and also slide actions make it somewhat ambiguous.  Counting trees or tracking the point might be easier.  A `children` method for sequences works as long as sequences actually implement it.


### Improper Levels

Children with no parents or missing a level are currently not supported and likely cause bad behavior.


# Thanks & Acknowledgments

This package is a direct descendant of Takaaki ISHIKAWA's [org-tree-slide](https://github.com/takaxp/org-tree-slide) package.  Many of the ideas and some of the implementations were either inherited or inspired by ideas from that package.  This package would not exist without the inspiration.  Thanks to everyone who contributed on org-tree-slide.

