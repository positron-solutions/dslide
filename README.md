<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit README.org -->

https://github.com/positron-solutions/dslide/assets/73710933/06a66e42-a172-48ba-968f-5f5b1989a868

<a href="https://melpa.org/#/dslide"><img src="https://melpa.org/packages/dslide-badge.svg" alt="melpa package"></a> <a href="https://stable.melpa.org/#/dslide"><img src="https://stable.melpa.org/packages/dslide-badge.svg" alt="melpa stable package"></a>
<a href="https://elpa.nongnu.org/nongnu/dslide.html"><img src="https://elpa.nongnu.org/nongnu/dslide.svg" alt="Non-GNU ELPA"></a>


# Present Org Documents 🦄

-   Per-heading configurable behavior
-   Nested or independent child slides
-   Header with breadcrumbs generated from document keywords
-   Actions that consume typical org data in smart ways


## Fully Programmable  ✨

-   Script steps in your presentation with Org babel blocks
-   Incorporate **anything** Emacs does into a presentation
-   Convenient API for quickly writing reliable custom actions for reuse


## Status 🛠️

Version 0.5.4 👷 Subscribe to Positron's [YouTube](https://www.youtube.com/@Positron-gv7do) for updates and related demonstrations.

-   Stabilizing the API and naming (estimated 98% stable)
-   Gathering user feedback & experience to pinpoint actual use patterns
-   Accepting PR's and issue reports
-   Some behaviors may be advertised as working already when they are only 90% implemented. I have no idea what you want. **File issues**.


# Installation

```elisp
;; From MELPA or ELPA
(use-package dslide)

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
```


## Try It Out

With just defaults, run `dslide-deck-start` on your existing documents. You can load the examples in the [./test/demo.org](./test/demo.md) file to see a showcase of configuration behavior.

The default keymap uses arrow keys. Left and right are `dslide-deck-forward` and `dslide-deck-backward`. Up is `dslide-deck-start` and will show the contents. Down is `dslide-deck-stop` and will stop the slide show.


# Features


## Simple User Interface

Fully programmable sequences behind a two-button presentation interface:

-   `dslide-deck-forward`
-   `dslide-deck-backward`


## Contents Navigation

Call `dslide-contents` to show a contents overview. Calling `dslide-deck-forward` and `dslide-deck-backward` in the contents can quickly move through headings. Call `dslide-deck-start` again to resume the presentation from that point.


## Narrate While Presenting

Check out `dslide-deck-develop`. You can see your hidden comments and the approximate progress indications. Babel actions will highlight blocks as they execute, showing you what just happened.


## Hide Markup

By default, the `dslide-action-hide-markup` action is configured in `dslide-default-actions`. Looks clean out of the box. Commented and `:noslide:` or `:noexport:` headings are filtered. Todos and tags are hidden.


## Independent Buffer State

The actual display is done in an indirect buffer. Your hooks and customizations for presentation will not pollute your editing buffer. Dirty state will not pile up in your presentation buffer, greatly increasing reliability even if your custom Elisp scripting is sloppy 💩.


# Table of Contents

-   [Creating](#Creating)
    -   [Actions](#Actions)
    -   [Adding Actions](#Adding-Actions)
    -   [Action Arguments](#Action-Arguments)
    -   [Annotating Elements](#Annotating-Elements)
    -   [Babel Scripting](#Babel-Scripting)
    -   [Hiding Markup](#Hiding-Markup)
    -   [Filtering Headings](#Filtering-Headings)
    -   [Header Keywords](#Header-Keywords)
    -   [File Local Variables](#File-Local-Variables)
-   [Presenting](#Presenting)
    -   [Two Button Interface](#Two-Button-Interface)
    -   [Contents Interface](#Contents-Interface)
    -   [Narrating](#Narrating)
    -   [Cursor Visibility](#Cursor-Visibility)
-   [Configuring](#Configuring)
    -   [Binding](#Binding)
    -   [Hooks](#Hooks)
    -   [Steezing Org](#Steezing-Org)
-   [Extending](#Extending)
    -   [Creating Actions](#Creating-Actions)
    -   [A Custom Action](#A-Custom-Action)
    -   [Default Classes](#Default-Classes)
-   [Hacking](#Hacking)
    -   [Objects & Ownership](#Objects-&-Ownership)
    -   [Stateful Sequence](#Stateful-Sequence)
    -   [Flow Control](#Flow-Control)
    -   [Instantiating Slides](#Instantiating-Slides)
-   [Package Pairings](#Package-Pairings)
    -   [Org Modern](#Org-Modern)
    -   [Org Appear](#Org-Appear)
    -   [Master of Ceremonies](#Master-of-Ceremonies)
    -   [Open Broadcaster Software](#Open-Broadcaster-Software)
    -   [moom.el](#moom.el)
-   [Contributing](#Contributing)
    -   [Work In Progress 🚧](#Work-In-Progress-🚧)
-   [Thanks](#Thanks)


# Creating

Making an org document into a presentation.


## Actions

By default, you just get one slide per heading, a header, and some animation. This is not very exciting. You need to add actions to slides to consume their section content in a more engaging way.

There are two kinds of actions:

-   `Slide actions`: mostly responsible for narrowing to a slide and then handling the child headings, either inline or as separate slides
-   `Section actions`: work on the content in the heading's section.

To browse all actions, because they are all EIEIO classes, you can use `eieio-browse` and see the actions descend from `dslide-action`.

```
+--dslide-stateful-sequence
     +--dslide-action
          +--dslide-action-propertize
          +--dslide-action-image
          +--dslide-action-babel
          +--dslide-action-item-reveal
          +--dslide-action-hide-markup
          +--dslide-slide-action
               +--dslide-slide-action-every-child
               +--dslide-slide-action-inline
               +--dslide-slide-action-child
               +--dslide-slide-action-flat
```


## Adding Actions

By default, every slide has two actions, configurable in `dslide-default-actions`:

-   `dslide-action-propertize` for adding text properties to arbitrary elements
-   `dslide-action-hide-markup` to hide keywords, todo states, and tags, allowing you to have these things in your source without them cluttering the presentation

Actions must be added to a slide using the slide's property drawer.

-   `DSLIDE_SLIDE_ACTION`: Usually narrows to the slide and creates children from child headings. Lifecycle encloses the section actions.
-   `DSLIDE_ACTIONS:` Most commonly customized. You can list multiple actions. Each one will step through its forward and backward steps.

Regular Org Mode markup is used to add actions to headings.  See more examples in the [[../test]] directory.

```org
* Full Screen Images
:PROPERTIES:
:DSLIDE_ACTIONS: dslide-action-images
:END:
#+attr_html: :width 50%
[[./images/emacsen4.jpeg]] [[./images/before-google3.jpeg]]
```


## Action Arguments

Many actions understand arguments, allowing tuning of similar behaviors from the same class.

To view an action's arguments, call `describe-symbol` on it. Any slot definition usually has the same `:initarg` and will be understood when added as a plist-style argument.

Configuring the slot is done by adding plist-style properties after the class name:

```org
:PROPERTIES:
:DSLIDE_ACTIONS: dslide-action-item-reveal :inline t
:END:
```

You can also use "property+" syntax to add to a property, and these accept plist arguments too:

```org
:PROPERTIES:
:DSLIDE_ACTIONS: dslide-action-babel
:DSLIDE_ACTIONS+: dslide-action-images :full-frame t
:END:
```

🚧 The current plist read implementation splits the string rather than using `read-string` and is therefore not smart enough to parse lists as arguments. However `dslide-action-propertize` demonstrates doing this correctly and shows that it will be possible if needed.


## Annotating Elements

Some actions, such as `dslide-action-propertize`, can't decide which elements to operate on or what to do with the element. You can add some meta data to an element using an **affiliated keyword**.

⚠️ If you are extending an action and want to create your own affiliated keyword, they must start with `attr` or else the parser will not consider them affiliated!

```org
* Fancy Text
:PROPERTIES:
:DSLIDE_ACTIONS: dslide-action-propertize
:END:
Add text properties to an element using the =attr_dslide_propertize= affiliated keyword.  No quoting is required.  Lists will be interpreted as such.

#+attr_dslide_propertize: face (:background "#ddddff" :foreground "#000000" :weight bold :height 2.0)
This is some fancy text
```


## Babel Scripting

You can write custom scripts into your presentation as Org Babel blocks. These can be executed with the `dslide-action-babel` action.

By default blocks only execute going forward, one block per step. You need to label your blocks with lifecycle methods if you want to be able to go forwards and backwards or execute them at the beginning or end of a slide. See the `dslide-action-babel` class and examples in [./test/demo.org](./test/demo.md).

The `#+attr_dslide:` affiliated keyword is used to configure which methods will run the block. Block labels that are understood:

-   `begin` and `end` are run when the slide is instantiated, going forward and backward respectively. You can have several blocks with these methods, and they will be run from **top-to-bottom** always, making it easier to re-use code usually.

-   `final` is only called when no progress can be made or if the presentation is stopped.

-   `forward` and `backward` are self-explanatory. Position your `backward` blocks **above** any block that they undo

-   `both` runs either direction. It will not repeat in place when reversing. Use seperate `forward` and `backward` blocks for that 💡

The babel action also understands regular babel options such as `:exports` and `:results`. Exports none will make the block invisible. Results controls whether results will be printed into the buffer or not.


### Step Callbacks

See `dslide-push-step` for inserting arbitrary callbacks that can function as steps. Unless your action performs state tracking to decide when to consume `dslide-deck-forward` and `dslide-deck-backward` itself, a callback may be easier. Using `dslide-push-step` is also one way to optionally add a step callback from a babel block.


## Hiding Markup

Dslide uses a lot of markup that would not look good in a presentation. It also filters it by default using `dslide-action-hide-markup`. You can adjust the types using `dslide-hide-markup-types`


### Hiding Todos and Tags

`dslide-action-hide-markup` will also hide todos and tags. You can modifiy this with `dslide-hide-todo` and `dslide-hide-tags`.


## Filtering Headings

-   Any heading with `COMMENT` directly after the stars will be skipped
-   Any heading with the `:noslide:` or `:noexport:` tags will be skipped

Use this when your headings are work-in-progress and you run out of time on Friday before the feature demo meeting. Have some content that is used only in some exports? Use `:noslide:`.


## Header Keywords

If `dslide-header` is configured, the keywords for the document title, email, and author etc will be used to generate an okay header.

```org
#+,title:	Domain Specific sLIDEs
#+author:	Positron
#+email:	contact@positron.solutions
```


## File Local Variables

Don't forget that if you need a customize variable only set in a particular presentation, you can use file local variables. Not every setting needs a keyword or babel block integration.

```org
# Local Variables:
# dslide-header: nil
# End:
```


# Presenting

How to control and view your presentation.


## Two Button Interface

Presentations tend to be organized into a scripted linear sequence. We want to control the entire presentation sequence mostly with two buttons, forwards and backwards.

The controllers for presenting usually have very few buttons. Dslide was designed with this usage pattern in mind and can mostly be controlled by two commands.

-   `dslide-deck-forward`

-   `dslide-deck-backward`

Many controllers also have a "play" button or similar. It's recommended to map this to `dslide-deck-start`.

🚧 It is intended to overload `dslide-deck-start` further to implement "secondary" actions that can be triggered non-linearly.

There is likely no good place to bind `dslide-deck-stop`, but it's not critical. You can do everything with just three buttons.


## Contents Interface

Navigate your presentation faster when answering questions. The contents interface is a view of top-level headings. It overloads the presentation controls to navigate.

To enter the contents, call `dslide-deck-start` when a presentation is already active.

-   `dslide-deck-start` will resume the presentation at that heading
-   `dslide-deck-stop` will similarly exit the contents view
-   `dslide-deck-forward` and `dslide-deck-backward` move between top level headings.


## Narrating

The presentation you see is a cloned [indirect buffer](info:elisp#Indirect Buffers) of your org mode buffer. The Elisp state and overlays are independent. There are two key advantages:

-   Any state you create in the presentation will not pollute the org mode buffer you are editing
-   We can display the source for the presentation simultaneously

`dslide-deck-develop` will attempt to display both the presentation and source simultaneously. Whenever the source is visible, highlights will be applied to indicate where the presentation is at. **This is especially helpful for including presentation notes in comments, which are hidden by default**.

To leave a comment for yourself in the presentation source, just add a comment block or comment line:

```org
# This is also a comment

#+begin_comment
This is a comment that only I can see while presenting, only when I look at my base buffer while sharing another frame.
#+end_comment
```

You can also switch a window to the base buffer manually. That's almost all `dslide-deck-develop` does.


## Cursor Visibility

By default, the cursor is hidden in the presentation buffer using `dslide-cursor-hide`. You can call `dslide-cursor-restore` if you need it.


# Configuring

Be sure to check `M-x` `customize-group` `dslide` to see all declared custom variables. All of the variables are configured to recommended defaults except hooks, which would depend on other packages usually.

Many settings can be configured at:

-   global level through customize variables
-   document level through keywords
-   slide level through the property drawer


## Binding

You likely want to start the mode via `dslide-deck-start`. Once the mode starts, it creates an indirect buffer to display the slides and then calls `dslide-deck-start-function` once the mode is active and everything is initialized, so you can customize startup behavior.

All commands begin with `dslide-deck` 💡

```elisp
(keymap-set org-mode-map "<f5>" #'dslide-deck-start)
```

Once the global minor mode, `dslide-mode` is active, additional bindings in `dslide-mode-map` are active in every buffer so that you can integrate other buffers into your presentation. (Tracking which buffers are part of a presentation is still a topic under consideration 🚧)


### Secondary Commands 🚧

Because you might want to play a video or take a branch in the presentation and then exit that branch, the plan is to overload the `dslide-deck-start` binding within presentations to enter / exit these branches.


## Hooks

Beware of using the normal `dslide-mode-hook` 😱 because it runs **in the base buffer** ⚠️. If you remap faces or add a bunch of styling, it will be copied to the indirect buffer but then linger in your base buffer. Instead, use `dslide-start-hook`. 💡

-   `dslide-start-hook` is run in the indirect buffer after it is set it. This is what you want.
-   `dslide-stop-hook` is run in the base buffer because the indirect buffer is already dead.
-   `dslide-contents-hook` is run after switching to contents. It runs in the slide buffer.
-   `dslide-narrow-hook` is run after narrowing, usually after a slide is started
-   `dslide-after-last-slide-hook` is run when the user tries to go forward but there are no more slides. You can use this to implement a final feedback before quitting or add `dslide-deck-stop` to exit without feedback.
    
    Another option is to use `dslide-push-step` to push a callback that will only run when called going forward.

```elisp
(defun my-stop-if-forward ()
  (dslide-push-step (lambda (direction)
                  (when (eq direction 'forward)
                    ;; Be sure to return t or the callback won't count as a
                    ;; step and the hook will run again.
                    (prog1 t (dslide-deck-stop))))))

(setq dslide-after-last-slide-hook #'my-stop-if-forward)
```


### Per-Slide Actions

💡 If you want to do something on each slide or specific slides, before using hooks, instead consider using actions.

See the `dslide-action-hide-markup` which is by default added to `dslide-default-actions` and hides markup on every slide. The lifecycle of actions and their methods for obtaining the current slide's heading make them very good for per-slide behavior.


## Steezing Org

Not unique to dslide, if you want more professional looking results, you will likely need to make your org a bit prettier.

The setup used for the Positron's YouTube demos is not much more complex than this well-documented setup by [System Crafters](https://systemcrafters.net/emacs-tips/presentations-with-org-present/). Also see Prot's [further](https://protesilaos.com/codelog/2020-07-17-emacs-mixed-fonts-org/) documentation on customizing org mode faces and fonts.

In short, use:

-   `org-modern`
-   `org-appear`
-   `nerd-icons` for more cheesy (Emacs logo)
-   And set the faces for org headings and document title.

Don't forget built-in `emoji-search` and searching `insert-char`.

Positron is cheating and also apply custom line-spacing and line-height. While Psionic maintains a custom `org-modern`, using custom spacing everywhere fights with `visual-line-mode` currently.


# Extending

This section is intended to provide an overview for extending dslide classes or hacking on dslide itself.


## Creating Actions

Actions are the right choice when you need custom behavior that you want to re-use. Actions can be configured with arguments. They implement the stateful sequence lifecycle. For one-off solutions, you probably just want a babel block.

First choose your action type:

-   Override `dslide-slide-action` to create a slide action. Your action will control the display of the slide and its children, usually controlling the narrow state and adding or removing overlays from children.

-   Override `dslide-action` to create an action that works mainly on a heading's section content.

Override methods as appropriate, configure a heading to use your action, and you're done. Some actions, such as `dslide-action-propertize` only work when some of the section data is annotated.


## A Custom Action

The `dslide-section-next` and `dslide-section-previous` method documentation are very helpful behavior for quickly writing custom actions. They advance the action's `:marker` forwards and backwards to the next matching element and return that element so we can do something with it.

-   declare a class
-   override a few methods
-   now you too can paint the paragraphs red

Example code:

```elisp
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
```


## Default Classes

The deck and slide class as well as actions can be sub-classed. Use the existing sub-classes of actions as example code for writing other classes. See the [eieio#Top](info:eieio#Top) manual for explanation of OOP in Elisp.

-   `Action`: Creating new action subclasses are an efficient way to perform similar operations on typical kinds of org data.
-   `Slide:` Slides can be configured extensively by changing their actions. However, for more vertical cooperation between slides or cooperation among actions, extended slides could be useful.
-   `Deck`: If the core methods of the deck are insufficient, extension is another option besides advice, hooks, and modifying the source.
    
    If you suspect you might need to sub-class the `dslide-slide` or `dslide-deck`, please file an issue because your use case is probably interesting.


# Hacking

This section provides really high-level summary of the code's major design choices to prepare for diving into source.


## Objects & Ownership

Org mode uses trees. Presentations are linear sequences. We can either traverse the tree or flatten it. Dslide chose to traverse. This design allowed implementing features such as `dslide-slide-action-each-child`. The children of such a parent slide exist simultaneously. A consequence of the choice not to flatten is that parents own their children. The lifecycle of a parent always encompasses its child.

-   The deck object is the root of all functionality and many commands delegate through it
-   The deck owns slides, which own actions
-   Slide actions may further own child slides


## Stateful Sequence

Presentations are supposed to be linear sequences. However, they may require setup and teardown. This is the "stateful" part of a `dslide-stateful-sequence`.

If all sequences were idempotent, we would just implement `dslide-forward` and `dslide-backward`. The reason this was not done is because those methods would have to differentiate calls to perform setup versus attempting to make progress and counting as steps. It was extremely tricky, and so setup and teardown were split into three methods.

This setup and teardown can happen in both directions, so there is `dslide-begin` and `dslide-end`. The latter commonly calls the former and then advances the state to the end, but some more optimal setups are possible and already in use.

Slides may be disposed of after they no longer make progress. To ensure this finalization happens, the parent calls `dslide-final`.


### Actions

Actions live on the slide. They implement stateful sequence. There are two kinds of actions:

-   Action: A regular action usually works on the section contents.
-   Slide Action: A slide action usually will narrow to its own contents. It can create new slides by calling `dslide--make-slide`, where it can override them to prevent them from narrowing. The slide action is always called before any other action, whether going in forward or reverse.


## Flow Control

Decks, slides, and actions implement the `dslide-stateful-sequence` interface. On each call to `dslide-deck-forward` or `dslide-deck-backward`, the deck receives the first call to its `dslide-forward` method.

First, the deck will check for any step callbacks. These are added with `dslide-push-step`. The deck delegates this to the slide. The slide may delegate down to an action, which may delegate to a slide.

In the most basic case, each delegate will try all of its actions until one returns non-nil. The delegate returns the first non-nill result, indicating that progress was made. If the delegate returns nil, it means it was unable to make progress, and so the caller will instead try its own next action.

Whenever all slides and actions return nil all the way back up to the deck, it looks for a next or previous top-level heading to make into a slide. If none is found, it indicates that the user is at the beginning or end of the presentation.

The deck object and slide actions frequently create new children from org headings. They call their `dslide-begin` or `dslide-end` methods right after that. If these methods don't indicate progress, the `dslide-forward` or `dslide-back` method will be called.


## Instantiating Slides

Slides are created by calling `dslide--make-slide` with an org element for a heading. This function interprets the class name and arguments for the new slide and instantiates the object.

The default classes and actions can be configured at the document or customize level. Set the `DSLIDE_DECK_CLASS` and `DSLIDE_SLIDE_CLASS` as well as other properties that work at the heading level. The order of precedence (**Not fully implemented** 🚧):

-   Property definition of the current heading
-   Property definition in the document
-   Customize variable

`dslide--make-slide` will look in order for the highest precedence setting and then instantiate the class with that value in the slot.


# Package Pairings

These are some packages that are likely to find use alongside dslide.


## Org Modern

Bullets and many prettifications of common org markups. The markup that you don't hide looks better with org modern.


## Org Appear

Never worry about turning on pretty links for a presentation. Edit them by just moving the point inside.


## Master of Ceremonies

The [master-of-ceremonies](https://github.com/positron-solutions/master-of-ceremonies) package is primarily used for its `moc-focus` command which isolates small snippets of buffer text to make fullscreen and pretty. You can save replay these snippets without having access to a source buffer.

Check the full commands by pressing `h` during focus. You can highlight a region, save an expression to playback a code snippet without the buffer open etc.


## Open Broadcaster Software

Sacha Chua has written an OBS plugin integration helpful for video integration [obs-websocket-el](https://github.com/sachac/obs-websocket-el).


## moom.el

The [moom](https://github.com/takaxp/moom#org-mode-org-tree-slide) package contains some commands for resizing text and repositioning frames.


# Contributing

-   Since you likely just need something to magically happen, the recommended option is to place a hamburger in the [hamburger jar](https://github.com/sponsors/positron-solutions) and file an issue.
-   If you do have time, excellent. Happy to support your PR's and provide context about the architecture and behavior.


## Work In Progress 🚧

Open issues and give feedback on feature requests. Contributions welcome.


### Slide Action Precedence

When a slide is created in `dslide-make-slide`, it can obtain them from several places:

-   passed in arguments, as slide actions do to prevent children from trying to display themselves
-   properties, how slides are usually configured
-   customize variables that set the default actions.

The order of precedence and capability to override options is still pretty immature.


### Secondary Commands

See the section about bindings for context. Video play or other situations where the presentation might branch should be supported by overloading the behavior of `dslide-deck-start`. I think this command will turn into `dslide-deck-secondary` in the `dslide-mode-map`.


### `dslide-goto`, starting from point

Since not many actions currently have implemented this very accurately, playing from point is likely not that accurate. Progress updating in the base buffer is also currently only at the slide level of granularity.


### Affiliated Buffers

There is no tracking whether a buffer is part of the presentation or not. How would a buffer become one? Should it be implicit? Without any sort of tracking, the consequence is that having a presentation open leaves the minor mode bindings hot. These commands do weird things when run from these situations, especially if running babel scripts, so some kind of first-class buffer affiliation seems necessary.


### Non-Graphic Display

For terminals, the line-height based slide-in effect is not supported.


### Improper Levels

Children with no parents or missing a level are currently not supported and likely cause bad behavior.


### Counting Slides

Especially if slides launch sub-sequences, and they do it from Lisp, this is hard. Buffer slides and also slide actions make it somewhat ambiguous. Counting trees or tracking the point might be easier. A `children` method for sequences works as long as sequences actually implement it.


### Non-Org Sequences

There's no concrete reason why presentations need to start with Org mode buffers. The deck object could have its org-specific functionality pushed down to an org-mode class. The only requirement is to be able to hydrate some stateful sequences, which may hydrate and call into sub-sequences, meaning anything is pretty trivially possible.


# Thanks

This package is a direct descendant of Takaaki ISHIKAWA's [org-tree-slide](https://github.com/takaxp/org-tree-slide) package. Many of the ideas and some of the implementations were either inherited or inspired by ideas from that package. This package would not exist without the inspiration. Thanks to everyone who contributed on org-tree-slide.
