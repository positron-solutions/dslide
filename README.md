<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit README.org -->

<a id="org022ed0d"></a>

# Domain Specific sLIDEs

https://github.com/positron-solutions/dslide/assets/73710933/06a66e42-a172-48ba-968f-5f5b1989a868

<a href="https://melpa.org/#/dslide"><img src="https://melpa.org/packages/dslide-badge.svg" alt="melpa package"></a> <a href="https://stable.melpa.org/#/dslide"><img src="https://stable.melpa.org/packages/dslide-badge.svg" alt="melpa stable package"></a>
<a href="https://elpa.nongnu.org/nongnu-devel/dslide.html"><img src="https://elpa.nongnu.org/nongnu-devel/dslide.svg"></a>
<a href="https://elpa.nongnu.org/nongnu/dslide.html"><img src="https://elpa.nongnu.org/nongnu/dslide.svg" alt="Non-GNU ELPA"></a>


<a id="orge34b71f"></a>

## Programmable Org Presentation 🦄

-   Per-element configurable behavior through extensible actions
-   Script steps in your presentation with **Org babel blocks**
-   Incorporate anything Emacs does with **keyboard macro playback**
-   Convenient API for quickly writing reliable custom actions for reuse
-   Decent out-of-the-box results with existing org documents

<a id="orgc2a4f79"></a>

### Status 💹

Everything in [./test/demo.org](https://github.com/positron-solutions/dslide/blob/master/test/demo.org) is maintained. File issues.

-   Still pre-1.0. See the [version 1.0 feature roadmap](https://github.com/positron-solutions/dslide/issues/20). Read the NEWS.org file for changes.
-   Expect less markup over time. Old markup should warn.
-   Accepting PR's. Read the manual section on [hacking](#orgc3245ea).

Emojis indicate work-in-progress 🚧, intended deprecation ⛔, or experimental features 🧪.

Subscribe to Positron's [YouTube](https://www.youtube.com/@Positron-gv7do) for updates and related demonstrations.


## Installation

```elisp
;; From MELPA or ELPA
(use-package dslide)

;; package-vc
(package-vc-install
 '(dslide
   :url "https://github.com/positron-solutions/dslide.git"))

;; using elpaca's with explicit recipe
(use-package dslide
    :ensure (dslide :host github
                    :repo "positron-solutions/dslide"))

;; straight with explicit recipe
(use-package dslide
    :straight (dslide :type git :host github
                      :repo "positron-solutions/dslide"))

;; or use manual load-path & require, you brave yak shaver
```


### Try It Out

[Clone](https://github.com/positron-solutions/dslide.git) the repo and then open the [./test/demo.org](https://github.com/positron-solutions/dslide/blob/master/test/demo.org) file. Call `dslide-deck-start`. The presentation will explain everything else while demonstrating Dslide.

You will need VLC installed and the [Master of Ceremonies](https://github.com/positron-solutions/moc) package to successfully run all examples, but you can skip them in the contents view or start at point on other examples to go around them.


## Features


### Simple User Interface

Fully programmable sequences behind a two-button presentation interface:

-   `dslide-deck-forward`
-   `dslide-deck-backward`

The default `dslide-mode-map` uses arrow keys. Left and right are `dslide-deck-forward` and `dslide-deck-backward`. Up is `dslide-deck-start` and will show the contents. Down is `dslide-deck-stop` and will stop the slide show.


### Contents Navigation

Call `dslide-contents` to show a contents overview. Calling `dslide-deck-forward` and `dslide-deck-backward` in the contents can quickly move through headings. Call `dslide-deck-start` again to resume the presentation from that point.


### Narrate While Presenting

Check out `dslide-deck-develop`. You can see your hidden comments and the approximate progress indications. Babel actions will highlight blocks as they execute, showing you what just happened.


### Hide Markup

By default, the `dslide-action-hide-markup` action is configured in `dslide-default-actions`. Looks clean out of the box. Commented and `:noslide:` or `:noexport:` headings are filtered. Todos and tags are hidden.


### Independent Buffer State

The actual display is done in an indirect buffer. Your hooks and customizations for presentation will not pollute your editing buffer. Dirty state will not pile up in your presentation buffer, greatly increasing reliability even if your custom Elisp scripting is sloppy 💩.


# Table of Contents

- [Domain Specific sLIDEs](#Domain-Specific-sLIDEs)
  - [Programmable Org Presentation 🦄](#Programmable-Org-Presentation-🦄)
    - [Status 💹](#Status-💹)
  - [Creating ✏️](#Creating-✏️)
    - [Actions 🪄](#Actions-🪄)
    - [Enabling Actions 🚦](#Enabling-Actions-🚦)
    - [Configuring Actions 🎛️](#Configuring-Actions-🎛️)
    - [Babel Scripting 🧑‍💻](#Babel-Scripting-🧑‍💻)
    - [Keyboard Macros 🤖](#Keyboard-Macros-🤖)
    - [Hiding Markup 🥷🏿](#Hiding-Markup-🥷🏿)
    - [Filtering Headings 🚮](#Filtering-Headings-🚮)
    - [Header Configuration 🎩](#Header-Configuration-🎩)
    - [File Local Variables 🍦](#File-Local-Variables-🍦)
  - [Presenting 📽️](#Presenting-📽️)
    - [Two Button Interface](#Two-Button-Interface)
    - [Contents Interface](#Contents-Interface)
    - [Source Following](#Source-Following)
    - [Cursor Visibility 🥷](#Cursor-Visibility-🥷)
  - [Configuring 🎛️](#Configuring-🎛️)
    - [Key Bindings](#Key-Bindings)
    - [Hooks](#Hooks)
    - [Steezing Org 🕶️](#Steezing-Org-🕶️)
  - [Extending 🧑‍🏭](#Extending-🧑‍🏭)
    - [Creating Actions](#Creating-Actions)
    - [A Custom Action](#A-Custom-Action)
    - [Default Classes](#Default-Classes)
  - [Hacking 🧑‍🔬](#Hacking-🧑‍🔬)
    - [Objects & Ownership](#Objects-&-Ownership)
    - [Stateful Sequence](#Stateful-Sequence)
    - [Instantiating Slides](#Instantiating-Slides)
    - [Display](#Display)
    - [Element Mapping](#Element-Mapping)
    - [Progress Tracking](#Progress-Tracking)
  - [Package Pairings](#Package-Pairings)
    - [Master of Ceremonies](#Master-of-Ceremonies)
    - [Org Modern](#Org-Modern)
    - [Org Appear](#Org-Appear)
    - [Open Broadcaster Software](#Open-Broadcaster-Software)
    - [moom.el](#moom.el)
  - [Contributing 🍔](#Contributing-🍔)
    - [Work In Progress 🚧](#Work-In-Progress-🚧)
  - [Acknowledgments 🥇](#Acknowledgments-🥇)

<a id="orgd426bd0"></a>

## Creating ✏️

Documents should "just work" and obtain decent results.

-   Add behavior to elements by enabling and configuring [actions](#org5f30c9b)
-   Add custom behavior with babel blocks and keyboard macros
-   Create custom actions to use different kinds of data in reusable ways


<a id="org5f30c9b"></a>

### Actions 🪄

Actions add behavior to your content. They can be configured per-slide and in some cases per-element.

There are two kinds of actions:

-   `Section actions`: work on the content in the heading's section. They use the `dslide-action` base class and prefix.
-   `Slide actions`: display the slide, usually by narrowing, and then handle the child headings, either inline or as independent slides. They use the `dslide-slide-action` base class and prefix.

To browse all actions, because they are all EIEIO classes, you can use `eieio-browse` and see the actions descend from `dslide-action`.

```
+--dslide-stateful-sequence
     +--dslide-action
          +--dslide-action-propertize
          +--dslide-action-image
          +--dslide-action-babel
          +--dslide-action-kmacro
          +--dslide-action-item-reveal
          +--dslide-action-hide-markup
          +--dslide-slide-action
               +--dslide-slide-action-every-child
               +--dslide-slide-action-inline
               +--dslide-slide-action-child
               +--dslide-slide-action-flat
```


<a id="org9458a69"></a>

### Enabling Actions 🚦

Most actions are enabled by scanning for the right content. The babel action is used on babel blocks. The image action is used on image links. Some actions, especially slide actions, must be explicitly enabled. The markup used to enable actions can also be where they are configured.

-   Per-Element

    Some actions, such as `dslide-action-propertize`, can't decide which elements to operate on or what to do with those elements. They are both enabled and configured per-element by using an **affiliated keyword**.
    
    ```org
    #+attr_dslide_propertize: face '(:foreground "#ff0000")
    This text will be red
    ```
    
    🚧 This is the preferred style of configuration moving forward.
    
    ℹ️ Affiliated keywords *must* have the `attr` prefix or they will not apply to the content they precede. Affiliated keywords cannot be attached to headings, which must use their property drawer to attach data.

-   Keyword

    For some actions, the configuration *is* the content. They use a keyword. The value of the keyword is the content for the action. As elsewhere, use plist `:key value` style configuration.
    
    ```org
    #+dslide_kmacro: :keys "M-x a n i m a t e <return>"
    ```

-   Property Drawer

    Some actions run on every element of the heading before you even see the content. Since there may be no associated content to attach them to, they can only be configured in the property drawer. Slide actions are always configured this way.
    
    ```org
    * Inline Children
    :PROPERTIES:
    :DSLIDE_SLIDE_ACTION: dslide-slide-action-child :header nil
    :DSLIDE_ACTIONS: dslide-action-item-reveal :inline t
    :END:
    - You won't believe these animations
    - This is the world's greatest presentation software
      + But mainly because it integrates with all you programming tools
    ```
    
    -   `DSLIDE_SLIDE_ACTION`: Accepts one slide action and its `:key value` configuration
    -   `DSLIDE_ACTIONS:` Can be used to list multiple action classes and their `:key value` configurations.
    
    The `dslide-action-hide-markup` action only runs when entering a slide, to hide markup before you see anything. 🚧 It will be configurable in the property drawer. Right now it checks `dslide-hide-markup-types`.
    
    🚧 These actions can currently only be configured in the property drawer but will be configured mainly per-element where possible in 0.7.0:
    
    -   `dslide-action-image`
    -   `dslide-action-item-reveal`

-   Default Actions ⛔

    ⛔ Every action will be "default" in 0.7.0. This concept still exists but the goal is to remove it. It has been nearly gotten rid of already.
    
    By default, every slide has five actions, configurable in `dslide-default-actions`. Non-default actions must be added to a slide using the slide's property drawer.
    
    ⚠️ Actions that work by recognizing org elements by type are perhaps a bit dangerous to leave on all the time. Some actions may both want to work on the same elements. This is why they are not all on by default.


<a id="org15d5eb2"></a>

### Configuring Actions 🎛️

Many actions understand configuration options, allowing tuning of similar behaviors from the same class.

💡 To view an action's default values, call `describe-symbol` on it. Any slot definition usually has the same `:initarg` and will be understood when used in the configuration.

Configuring is usually done by adding plist-style `:key value` arguments after the class name, keyword, or affiliated keyword:

```org
* A Headline For a Heading
:PROPERTIES:
# configuration after a class name
:DSLIDE_ACTIONS: dslide-action-item-reveal :inline t
:END:

# A keyword configuration
#+dslide_kmacro: :frequency 0.08 :jitter 0.5 :keys "M-x a n"

# An affiliated keyword configuration
#+attr_dslide_propertize: face '(:background "#ddddff")
This text will be propertized
```

🚧 After class names, the current plist read implementation splits the string rather than using `read-string` and is therefore not smart enough to parse lists as arguments. However `dslide-action-propertize` demonstrates doing this correctly and shows that it will be possible if needed.


<a id="org5b1415f"></a>

### Babel Scripting 🧑‍💻

You can write custom scripts into your presentation as Org Babel blocks. These are executed with the `dslide-action-babel` action. Easy peazy.

```org
* My Heading With Babel Blocks
#+begin_src elisp
  (message "Good job!")
#+end_src
```

-   Controlling Direction ♻️

    By default blocks only execute going forward, one block per step. You need to label your blocks with [lifecycle](#org6cbacb3) methods if you want to perform setup (can be forward or backward) and teardown. See the `dslide-action-babel` class and examples in [./test/demo.org](https://github.com/positron-solutions/dslide/blob/master/test/demo.org).
    
    The `:direction` babel block parameter is used to configure which methods will run the block. Block labels that are understood:
    
    -   `forward` and `backward` are self-explanatory. Position your `backward` blocks **above** any block that they undo
    
    -   `both` runs either direction. It will not repeat in place when reversing. Use separate `forward` and `backward` blocks for that 💡
    
    -   `begin` and `end` are run when the slide is instantiated, going forward and backward respectively. You can have several blocks with these methods, and they will be run from **top-to-bottom** always, making it easier to re-use code usually.
    
    -   `init` is a combination of `begin` and `end` to make it easier to write.
    
    -   `final` is called to clean up when no progress can be made or if the presentation is stopped.
    
    💡These methods follow the naming and behavior of dslide's [stateful sequence](#org891ac3e) interface. The babel action is basically delegating stateful sequence calls into the blocks of your org document.
    
    The `:direction` parameter goes after the block language.
    
    ```org
    #+begin_src elisp :direction backward
      (message "I run going backwards, on `dslide-deck-backward'")
    #+end_src
    ```
    
    You can use `[vector]` or `'(quoted list)` syntax to combine methods.
    
    ```org
    #+begin_src elisp :direction [end forward]
      (message "My configuration is a vector with multiple directions")
    #+end_src
    ```

-   Ignoring Blocks 🙅

    Use the `:eval` parameter to prevent evaluation of blocks that aren't for your presentation.
    
    ```org
    #+begin_src elisp :eval never
      (message "Who cares?  I am never evaluated")
    #+end_src
    ```

-   Visibility 👻

    The babel action also understands regular babel options such as `:exports` and `:results`. Exports none will make the block invisible. Results controls whether results will be printed into the buffer or not.
    
    ```org
    # Only the results of this block are visible
    #+begin_src elisp :exports results
      '(a b c)
    #+end_src
    ```
    
    🚧 Some `:exports` and `:results` values are possibly not supported or supported weirdly. Please, file issues 💁🍔

-   Confirming Evaluation 🔏

    By default, `org-confirm-babel-evaluate` is set to nil. This will drive you nuts during a presentation. You should set this to t either in your `dslide-start-hook` or in a [file local variable](#org0c6d65c).
    
    ☣️ Don't run random people's presentations without checking the source!

-   Step Callbacks 👟

    You can use `dslide-push-step` for inserting arbitrary callbacks that can function as steps. Like everything else in dslide, returning non-nil means progress was made and the step should be consumed. By adding these inside babel blocks, you can add extra steps that depend on the next direction.
    
    The callback function should accept a DIRECTION argument. DIRECTION is forward, backward, or nil. nil just means the presentation is ending or displaying the contents, so you should clean up instead of attempting to do work.
    
    ```org
    #+begin_src elisp
      (message "Just doing block things")
    
      ;; Let's also push a step!
      (dslide-push-step
       (lambda (direction)
         ;; Decide what to do
         (pcase direction
           ;; `message' returns non-nil and will function as a padding step
           (forward (message "Injecting an extra step"))
           ;; `prog1' nil returns the nil, so this will not add a step
           (backward (prog1 nil (message "No step for you!")))
           ;; The _ catch-all will handle non-directional calls, such as quitting
           (_ (prog1 nil (message "Cleaning 💩 up!"))))))
    #+end_src
    
    ```
    
    ℹ️ You can also use `dslide-push-step` in actions for implementing tricky action behaviors. The image action uses this currently.


<a id="org5dc1187"></a>

### Keyboard Macros 🤖

🧪 Experimental new feature. Hopefully the configuration argument names are good. Hopefully.

The `dslide-action-kmacro` will run pre-recorded sequences of keystrokes as if you are controlling the computer. Through `:frequency` and `:jitter`, it plays back strokes at a human-feeling pace.

Keyboard macros support :direction, but only forward and backward are recognized. Forward is the default.

By playing back keyboard macros, you can encode real Emacs workflows as steps in a presentation. Don't just talk about how your software works. Use the software with fully reproducible steps that users can understand in a tactile, human way.

To record kmacros as presentation steps, use the `dslide-kmacro-transcribe-set-mark` command. It will save a marker and every time you call `kmacro-end-macro`, it will transcribe that macro as an expression that `dslide-action-kmacro` knows how to play back.

🆒 The jitter uses a Laplace distribution to sample a perturbation power. This power is mapped onto the zero-to-infinity factor range by raising e to the power of jitter. This is multiplied by `:frequency`, which is a duration. As a result, while the jitter is usually pretty small, it does have some wild variation, which does look a bit more human.


<a id="orgac4ea3e"></a>

### Hiding Markup 🥷🏿

Dslide uses a lot of markup that would not look good in a presentation. It also hides it by default using `dslide-action-hide-markup`. You can adjust the types using `dslide-hide-markup-types`

`dslide-action-hide-markup` will also hide todos and tags. You can modify this with `dslide-hide-todo` and `dslide-hide-tags`.


<a id="org26efb3f"></a>

### Filtering Headings 🚮

-   Any heading with `COMMENT` directly after the stars will be skipped
-   Any heading with the `:noslide:` or `:noexport:` tags will be skipped

Use this when your headings are work-in-progress and you run out of time on Friday before the feature demo meeting. Have some content that is only not used in presentations? Use `:noslide:`.

To change the filtering from what is done by `dslide-built-in-filter`, customize `dslide-default-filter` or set `DSLIDE_FILTER` (possibly implemented 🤡, file an issue!).


<a id="org796fdf5"></a>

### Header Configuration 🎩

If `dslide-header` is configured, the keywords for the document title, email, and author etc will be used to generate an okay header.

```org
#+,#+title:	Domain Specific sLIDEs
#+author:	Positron
#+email:	contact@positron.solutions
```

You can try customizing with `dslide-header-email` and similar variables or just set `dslide-header-fun` to completely replace the header with your own device. Check its signature!

-   Breadcrumbs 🍞

    Whenever `dslide-breadcrumb-separator` is non-nil, breadcrumbs will be rendered in the heading, displaying parent headings so the audience an track context.
    
    `dslide-breadcrumb-separator-style` controls whether there is a separator after the terminal breadcrumb. This can really help distinguish the breadcrumb from the current heading's headline. `append` will have a terminal separator while `separate` will only put them between breadcrumbs.
    
    Because breadcrumb text comes from your headings, you may want to set a face on them to prevent various heading faces from leaking into the breadcrumbs.


<a id="org0c6d65c"></a>

### File Local Variables 🍦

Don't forget that if you need a customize variable only set in a particular presentation, you can use file local variables. Not every setting needs a keyword or babel block integration.

\#+cindex confirming evaluation This is also one good way to set `org-confirm-babel-evaluate` and other settings that are somewhat risky to leave on generally.

```org
# Local Variables:
# dslide-header: nil
# org-confirm-babel-evaluate: nil
# End:
```


<a id="org9fed641"></a>

## Presenting 📽️

How to control and view your presentation.


<a id="org9d21cf4"></a>

### Two Button Interface

Presentations tend to be organized into a scripted linear sequence. We want to control the entire presentation sequence mostly with two buttons, forwards and backwards.

The controllers for presenting usually have very few buttons. Dslide was designed with this usage pattern in mind and can mostly be controlled by two commands.

-   `dslide-deck-forward`

-   `dslide-deck-backward`

Many controllers also have a "play" button or similar. It's recommended to map this to `dslide-deck-start`.

🚧 It is intended to overload `dslide-deck-start` further to implement "secondary" actions that can be triggered non-linearly.

There is likely no good place to bind `dslide-deck-stop`, but it's not critical. You can do everything with just three buttons.


<a id="orgdaaf472"></a>

### Contents Interface

Navigate your presentation faster when answering questions. The contents interface is a view of top-level headings. It overloads the presentation controls to navigate.

To enter the contents, call `dslide-deck-start` when a presentation is already active.

-   `dslide-deck-start` will resume the presentation at that heading
-   `dslide-deck-stop` will similarly exit the contents view
-   `dslide-deck-forward` and `dslide-deck-backward` move between top level headings.


<a id="org0a51629"></a>

### Source Following

🚧 The start functions were recently overhauled. They need user feedback, both to identify bugs and focus on real uses cases. `dslide-deck-start` should be the most reliable way to start presentations.

The presentation you see is a cloned [indirect buffer](info:elisp#Indirect Buffers) of your org mode buffer. The Elisp state and overlays are independent. There are two key advantages:

-   Any state you create in the presentation (besides buffer text and text properties!) will not pollute the org mode buffer you are editing
-   We can display the source for the presentation simultaneously, making one pretty while leaving the other to show us what is hidden

Using comments and comment blocks, you can write down prompts or scripts to help you maintain your flow. Dslide highlights the current progress state, providing both debugging and narration feedback.

To leave a comment for yourself in the presentation source, just add a comment block or comment line:

```org
# This is also a comment

#+begin_comment
This is a comment that only I can see while presenting, only when I look at my base buffer while sharing another frame.
#+end_comment
```

-   Present

    `dslide-deck-present` will display the slide buffer in an entirely new frame. You can customize this frame via the `dslide-deck-present` hook.
    
    By using a frame, you can resize the frame and use frame specific `set-face-attribute` calls. Because the hook configuration is independent, your customizations for development and presenting don't have to fight each other.

-   Develop

    `dslide-deck-develop` will attempt to display both the presentation and source simultaneously using a window.
    
    The `dslide-deck-develop` hook is only called when using this command. Because the hook configuration is independent, your customizations for development and presenting don't have to fight each other.
    
    You can also switch a window to the base buffer manually. That's almost all `dslide-deck-develop` does.

-   Start Functions

    Dslide can be started by several commands. `dslide-deck-start` is the most obvious. Writing such a command is as simple as binding `dslide-start-function` and then activating the mode. The reason Dslide does this is to have several start commands that can basically inject their behavior like a user would with a hook except without messing with the user's hooks.
    
    When a start function is bound, `dslide-mode` will create the slide buffer and then run the start function in order to rearrange windows or frames etc. You can make your own start functions to start dslide in other ways. Just ask yourself if you want the same hook behavior all the time. If not, making a new command that uses a start function is a clean way.


<a id="org34be415"></a>

### Cursor Visibility 🥷

By default, the cursor is hidden in the presentation buffer using `dslide-cursor-hide`. Remove it from the `dslide-start-hook` to disable this. You can call `dslide-cursor-restore` if you just temporarily need a cursor.

Another good choice for interactive presentations is to use `moc-subtle-cursor-mode` from the [Master of Ceremonies](https://github.com/positron-solutions/moc) package. It is more like having a laser pointer that hides itself automatically.


<a id="org036d0d9"></a>

## Configuring 🎛️

Be sure to check `M-x` `customize-group` `dslide` to see all declared custom variables. All of the variables are configured to recommended defaults except hooks, which would depend on other packages usually.

Many settings can be configured at:

-   global level through customize variables
-   document level through keywords
-   slide level through the property drawer


<a id="orgdbbebca"></a>

### Key Bindings

You likely want to start the mode via `dslide-deck-start`. Once the mode starts, it creates an indirect buffer to display the slides and then calls `dslide-deck-start-function` once the mode is active and everything is initialized, so you can customize startup behavior.

💡 All top-level presentation commands begin with the `dslide-deck` prefix

```elisp
(keymap-set org-mode-map "<f5>" #'dslide-deck-start)
```

Once the global minor mode, `dslide-mode` is active, additional bindings in `dslide-mode-map` are active in every buffer so that you can integrate other buffers into your presentation. (Tracking which buffers are part of a presentation is still a topic under consideration 🚧)

-   Secondary Commands 🚧

    Because you might want to play a video or take a branch in the presentation and then exit that branch, the plan is to overload the `dslide-deck-start` binding within presentations to enter / exit these branches.


<a id="org55a5890"></a>

### Hooks

Beware of using the normal `dslide-mode-hook` 😱 because it runs **in the base buffer** ⚠️. If you use that hook to remap faces or add a bunch of styling, state will be copied to the indirect buffer but then linger in your base buffer. Instead, use `dslide-start-hook`. 💡

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

-   Per-Slide Behavior

    💡 If you want to do something on each slide or specific slides, before using hooks, instead consider using actions.
    
    See the `dslide-action-hide-markup` which is by default added to `dslide-default-actions` and hides markup on every slide. The lifecycle of actions and their methods for obtaining the current slide's heading make them very good for per-slide behavior.


<a id="orgaec7089"></a>

### Steezing Org 🕶️

This is not unique to dslide, but if you want more professional looking results, you will likely need to make your org a bit prettier.

The setup used for the Positron's YouTube demos is not much more complex than this well-documented setup by [System Crafters](https://systemcrafters.net/emacs-tips/presentations-with-org-present/). Also see Prot's [further](https://protesilaos.com/codelog/2020-07-17-emacs-mixed-fonts-org/) documentation on customizing org mode faces and fonts.

In short, use:

-   `org-modern`
-   `org-appear`
-   `nerd-icons` for more cheesy (Emacs logo)
-   And set the faces for org headings and document title.

Don't forget built-in `emoji-search` and searching `insert-char`.

Positron is cheating and also apply custom line-spacing and line-height. While Psionic maintains a custom `org-modern`, using custom spacing everywhere fights with `visual-line-mode` currently.


<a id="orgf0afa74"></a>

## Extending 🧑‍🏭

Creating new actions or replacing dslide classes.


<a id="org7b057bd"></a>

### Creating Actions

Actions are the right choice when you need custom behavior that you want to re-use. Actions can be configured with arguments. They implement the stateful sequence lifecycle. For one-off solutions, you probably just want a babel block.

First choose your action type:

-   Override `dslide-action` to create an action that works mainly on a heading's section content.

-   Override `dslide-slide-action` to create a slide action. Your action will control the display of the slide and its children, usually controlling the narrow state and adding or removing overlays from children.

Override methods as appropriate, configure a heading to use your action, and you're done. Some actions, such as `dslide-action-propertize` only work when some of the section data is annotated.


<a id="org7e87b78"></a>

### A Custom Action

The `dslide-section-next` and `dslide-section-previous` methods are very helpful behavior for quickly writing custom actions. They advance the action's `:marker` forwards and backwards to the next matching element and return that element so we can do something with it.

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


<a id="org50633ac"></a>

### Default Classes

The deck and slide class as well as actions can be sub-classed. Use the existing sub-classes of actions as example code for writing other classes. See the [eieio#Top](info:eieio#Top) manual for explanation of OOP in Elisp.

-   `Action`: Creating new action subclasses are an efficient way to perform similar operations on typical kinds of org data.
-   `Slide:` Slides can be configured extensively by changing their actions. However, for more vertical cooperation between slides or cooperation among actions, extended slides could be useful.
-   `Deck`: If the core methods of the deck are insufficient, extension is another option besides advice, hooks, and modifying the source.
    
    If you suspect you might need to sub-class the `dslide-slide` or `dslide-deck`, please file an issue because your use case is probably interesting.


<a id="orgc3245ea"></a>

## Hacking 🧑‍🔬

This section provides really high-level summary of the code's major design choices to prepare for diving into source.


<a id="org1ac334e"></a>

### Objects & Ownership

Org mode uses trees. Presentations are linear sequences. We can either traverse the tree or flatten it. Dslide chose to traverse. This design allowed implementing features such as `dslide-slide-action-each-child`. The children of such a parent slide exist simultaneously. A consequence of the choice not to flatten is that parents own their children. The lifecycle of a parent always encompasses its child.

-   The deck object is the root of all functionality and many commands delegate through it
-   The deck owns slides, which own actions
-   Slide actions may further own child slides

-   Life Cycles

    Owning an object also means out-living it. This is important to understanding the sequence of events. The methods used for the lifecycle are part of dslide's [Stateful Sequence](#org891ac3e). Every lifecyle starts with `dslide-begin` or `dslide-end` (depending on whether we go forward or backward) and ends with `dslide-final`.
    
    The state of the slide out-lives the state of its slide action. The slide action out-lives both child slides and section actions. Child slides and section actions life cycles may overlap.
    
    The child and section actions orders can vary depending on the slide action's choices. There may be multiple children alive at one time. The children may complete after, before, or at the same time as the section actions.
    
    **Going forward:**
    
    1.  slide `dslide-begin`
    2.  slide action `dslide-begin`
    3.  section actions + child slides `dslide-begin`
    4.  section actions + child slides `dslide-final`
    5.  slide action `dslide-final`
    6.  slide `dslide-final`
    
    **Going backward:**
    
    1.  slide `dslide-end`
    2.  slide action `dslide-end`
    3.  child slides + section actions `dslide-end`
    4.  child slides + section actions `dslide-final`
    5.  slide action `dslide-final`
    6.  slide `dslide-final`


<a id="org891ac3e"></a>

### Stateful Sequence

Presentations are supposed to be linear sequences. We want to traverse the sequence, performing the steps, entirely by calling `dslide-forward` and `dslide-backward`.

If all sequences were idempotent, we would only implement `dslide-forward` and `dslide-backward`. However, sequences often require setup and teardown before carrying out a single step. This is the "stateful" part.

Implementing this without explicit setup methods crammed too much behavior into `dslide-forward` and `dslide-backward` while also requiring them to decide if they were attempting to make progress or just performing setup. It was annoying when building actions.

Setup and teardown can happen in both directions, so there is `dslide-begin` and `dslide-end`. The latter commonly calls the former and then advances the state to the end, but some more optimal setups are possible and already in use.

Slides may be disposed of after they no longer make progress. To allow intended cleanup to happen at the right moment, the parent calls `dslide-final`. This can be called at any time after `dslide-end` or `dslide-begin`.

The return values for these methods matter! See [flow control](#org1b3214c).

-   Flow Control

    Decks, slides, and actions implement the `dslide-stateful-sequence` interface. On each call to `dslide-deck-forward` or `dslide-deck-backward`, the deck receives the first call to its `dslide-forward` method. First, the deck will check for any step callbacks. (These may be added with `dslide-push-step` in actions or babel blocks.) If there are no step callbacks, the deck delegates `dslide-forward` to the slide. The slide may delegate down to an action, which may then further delegate to a child slide and so on. Eventually, an action will implement the step.
    
    The return value tells the parent if progress was made. In the most basic case, each delegate will try all of its actions until one returns non-nil. The delegate returns the first non-nil result. If the delegate returns nil, it means it was unable to make progress, and so the caller will instead try its own next action. The deck will find a root level sibling and hydrate it using `dslide--make-slide`. A child action would find the next child and hydrate that.
    
    The moment of calling `dslide-final` can vary depending on the situation. If there is only one slide at a time, `dslide-final` is usually called right after the child returns nil. If the child is one of several, such as with `dslide-slide-action-inline`, then only after all children complete will they have their `dslide-final` called. If the presentation is quit early or the contents are opened, `dslide-final` is also called, possibly before all steps are complete.
    
    Whenever all slides and actions return nil all the way back up to the deck, it looks for a next or previous top-level heading to make into a slide. If none is found, it indicates that the user is at the beginning or end of the presentation.
    
    The deck object and slide actions frequently create new children from org headings. They call their `dslide-begin` or `dslide-end` methods right after that. If these methods don't indicate progress, the `dslide-forward` or `dslide-back` method will be called.


<a id="org0b7c910"></a>

### Instantiating Slides

Slides are created by calling `dslide--make-slide` with an org element for a heading. This function interprets the class name and arguments for the new slide and instantiates the object.

The default classes and actions can be configured at the document or customize level. Set the `DSLIDE_DECK_CLASS` and `DSLIDE_SLIDE_CLASS` as well as other properties that work at the heading level. The order of precedence (**Not fully implemented** 🚧):

-   Property definition of the current heading
-   Property definition in the document
-   Customize variable

`dslide--make-slide` will look in order for the highest precedence setting and then instantiate the class with that value in the slot.


<a id="org0d799da"></a>

### Display

How various visual effects are achieved.

-   Hiding

    The slide buffer, being an indirect clone of the base buffer, does not have independent text properties. For this reason, overlays are typically used to hide content.
    
    Most things that are hidden could be once again revealed. This is typically accomplished by mutating or deleting the overlay.

-   Animation

    There are currently two animation types, peel and slide-in:
    
    -   Peel uses an overlay that is removed from the content one character at a time. In order to preserve the flow of the obscured contents, such as when doing inline reveal with content after the reveal, the overlay must either match the background color or use the display property.
    -   Slide-in uses an overlay with a `:before-string` text property to insert a newline in order to use the `:line-height` property to slowly remove padding with a timer.
    
    Peel is the default when an action or slide's `:inline` property is non-nil. If multiple actions try to slide in at once, the result will not be good. This is visible when combining `dslide-slide-action-every-child` and `dslide-action-item-reveal`

-   Full Screen Images

    These are just image buffers with the mode line turned off.


<a id="orgdcd15ab"></a>

### Element Mapping

Org's Element API is the foundation on top of which dslide is built. It's documentation is not currently in a manual. Here's the web link: [Org Element API docs](https://orgmode.org/worg/dev/org-element-api.html). By using the element parser, we can avoid the issues that plague regex based implementations. (The trade-off is more garbage generation.)

Very frequently, we parse a section of the document and map over elements or headings within. This allows us to treat the document or a part of it as a list. The mapping functions all eventually delegate to `dslide--map` which itself uses `org-element-map`, narrowed to the targeted region.

It is very common when writing actions to work on only the section or only the children. For this reason, some shortcuts to map the section or children exist. Some section actions such as `dslide-action-hide-markup` are almost entirely built `on dslide-section-map`.

Frequently we are looking for an element before or after a marker, so shortcuts exist for finding the next or previous element. Section actions typically use `dslide-section-next` and `dslide-section-previous`. Slide actions typically use `dslide-child-next` and `dslide-child-previous` to traverse the child headings.

Mapping and [progress tracking](#org3e40561) are intimately related. Finding the previous or next element is implemented by mapping to find the element beginning before or after a certain point. Careful handling of markers and a consistent scheme for sensing progress enable markers in the buffer to act as progress cursors for a variety of actions.


<a id="org3e40561"></a>

### Progress Tracking

Dslide's predecessor, org-tree-slide, frequently used the point to track state. This can be fragile and there is also only one point. To be more robust when the document is changing out from under us, dslide uses markers.

Slides keep a reference to the heading in their `:begin` slot and then retrieve it using `org-element-at-point`. Actions similarly use a marker in order to keep track of how much of the current heading they have already used. For convenience, `dslide-section-next` and `dslide-section-previous` are used to simultaneously find the next element and update the marker, eliminating silly mistakes like forgetting to update the marker.

There are two schemes in place for tracking progress:

-   When viewing a sequence of images, we reverse by going back to the previous image rather than re-showing the current image. This is the default progress scheme.
-   When showing and hiding elements, we reverse undoing the most recent work. This means hiding the most recently shown or showing the most recently hidden element. This is the scheme used when REVERSE-IN-PLACE is non-nil.

⚠️ This section is fiddly and tricky. Put on your smarty hat. 👷

**Normal Progress**

In short, find the element beginning after (before in reverse) the marker, move the marker to its beginning, and work on that element. If there is no next element, move as far as you can.

-   The heading always begins before the first element. We can reliably position before all elements by putting the marker at the beginning of the heading.
-   To go forward, we find the first element beginning after the marker and move the marker to its beginning. We work on that element. It will be skipped if we immediately go backward again.
-   If there is no next element, we move the marker to the end of the heading, which is after the beginning of the last element.
-   To go backward, we find the first element beginning before the marker and move the marker to its beginning. We work on that element. It will be skipped if we immediately go forward again.

**Reverse In Place Progress**

If doing work means the next reverse step should undo that work, you need reversing in place. We need to slightly tweak our rules to allow two states on each element. Since every element ends after it begins, we can reliably use the end and beginning positions to differentiate if we already used an element when going forwards or backwards.

A very deliberate design choice was to avoid needing to return more than one element from a mapping call. This means we always want to find the element we intend to work on e.g. we do not want to find the element to work on and then have to find the next element to update the marker.

-   To go forward, we find the first element beginning at or after the marker. We move the marker to the end of this element and work on this element.
-   To go backward, we find the first element ending at or before the marker. We move the marker to the beginning of this element and work on this element.
-   Going forward, begin at the beginning of the heading just like normal progress. The first element will not be skipped.
-   Going backward, begin at the end of the heading. This is at most the end of the last element, so it won't be skipped.

☢️ Before these two schemes were developed, some actions were easier to implement one way while others were easier the other way. There was much flip-flopping and radiation sickness from broken actions. Eventually it was realized that both schemes make perfect sense for the right problems.

⚠️ Org elements can and do overlap. Lists are one such challenge. List elements can all end at the same location. Naively calling `org-element-at-point` is a bad idea. See `dslide-action-item-reveal` for higher level interfaces.

If you need more states per element, this kind of implicit state tracking is insufficient and you will have to implement state-tracking. ⚠️ Don't use text properties to store state in buffer text since they will persist in the base buffer between presentation starts if not cleaned up.


<a id="org8313e0b"></a>

## Package Pairings

These are some packages that are likely to find use alongside dslide.


<a id="orgd7f53ad"></a>

### Master of Ceremonies

-   `moc-dispatch` is a set of controls for screen recording.
-   `moc-focus` shows small excerpts from buffers fullscreen.
    -   It has a transient interface bound to `h`.
    -   You can highlight multiple segements of the excerpt and change display.
    -   It makes text pretty for taking screenshots and for captures.
    -   It generates playback expressions you can even use as slides.

[Master of Ceremonies](https://github.com/positron-solutions/moc) was written as a companion to dslide and was used in almost every single dslide demonstration video.


<a id="org791bdb6"></a>

### Org Modern

Bullets and many prettifications of common org markups. The markup that you don't hide looks better with org modern.


<a id="org4bf49f3"></a>

### Org Appear

Never worry about turning on pretty links for a presentation. Edit them by just moving the point inside.


<a id="org50c1f22"></a>

### Open Broadcaster Software

Sacha Chua has written an OBS plugin integration helpful for video integration [obs-websocket-el](https://github.com/sachac/obs-websocket-el).


<a id="orgbf0e624"></a>

### moom.el

The [moom](https://github.com/takaxp/moom#org-mode-org-tree-slide) package contains some commands for resizing text and repositioning frames.


<a id="orgb75bf7f"></a>

## Contributing 🍔

-   Since you likely just need something to magically happen, the recommended option is to place a hamburger in the [hamburger jar](https://github.com/sponsors/positron-solutions) and file an issue.
-   If you do have time, excellent. Happy to support your PR's and provide context about the architecture and behavior.


<a id="orgefcafc8"></a>

### Work In Progress 🚧

Open issues and give feedback on feature requests. Contributions welcome. See the [1.0 feature roadmap](https://github.com/positron-solutions/dslide/issues/20).

-   Affiliated Keyword

    This is the future of dslide. Currently adding behavior to content can frequently require adding actions to the property drawer and then adding a keyword to the content. This lacks precision, is unintuitive, and is inconvenient. `dslide-action-propertize` shows the way. A dispatch system should find all `dslide` prefixed affiliated keywords and activate the actions with the configuration. This is faster and more concise. It only requires editing in one place rather than two.
    
    The property drawer will remain in use because headings have slide behavior that doesn't make sense to adjust with affiliated keywords. For behavior affecting section elements or operating on objects within paragraphs, the affiliated keyword implementation is the right way.

-   Layout

    A centering action is in the works.
    
    Another option is using the [Master of Ceremonies](https://github.com/positron-solutions/moc) package and its `moc-focus` command implement desirable behaviors such as filling the available space and padding the content to the center of the window. This behavior could easily be improved and adapted into an action.

-   Action Configuration Precedence

    When a slide is created in `dslide-make-slide`, it can obtain them from several places:
    
    -   passed in arguments, as slide actions do to prevent children from trying to display themselves
    -   properties, how slides are usually configured
    -   customize variables that set the default actions.
    
    The order of precedence and capability to override options is still pretty immature.

-   Secondary Commands

    See the section about bindings for context. Video play or other situations where the presentation might branch should be supported by overloading the behavior of `dslide-deck-start`. I think this command will turn into `dslide-deck-secondary` in the `dslide-mode-map`.

-   Starting From Point

    Since not many actions currently have implemented `dslide-goto` very accurately, playing from point is likely not that accurate. Progress updating in the base buffer is also currently only at the slide level of granularity.

-   Affiliated Buffers

    There is no tracking whether a buffer is part of the presentation or not. How would a buffer become one? Should it be implicit? Without any sort of tracking, the consequence is that having a presentation open leaves the minor mode bindings hot. These commands do weird things when run from these situations, especially if running babel scripts, so some kind of first-class buffer affiliation seems necessary.

-   Non-Graphic Display

    For terminals, the line-height based slide-in effect is not supported.

-   Improper Levels

    Children with no parents or missing a level are currently not supported and likely cause bad behavior.

-   Counting Slides

    Especially if slides launch sub-sequences, and they do it from Lisp, this is hard. Buffer slides and also slide actions make it somewhat ambiguous. Counting trees or tracking the point might be easier. A `children` method for sequences works as long as sequences actually implement it.

-   Non-Org Sequences

    There's no concrete reason why presentations need to start with Org mode buffers. The deck object could have its org-specific functionality pushed down to an org-mode class. The only requirement is to be able to hydrate some stateful sequences, which may hydrate and call into sub-sequences, meaning anything is pretty trivially possible.


<a id="orge5c2378"></a>

## Acknowledgments 🥇

This package is a direct descendant of Takaaki ISHIKAWA's [org-tree-slide](https://github.com/takaxp/org-tree-slide) package. Many of the ideas and some of the implementations were either inherited or inspired by ideas from that package. This package would not exist without the inspiration. Thanks to everyone who contributed on org-tree-slide.
