---
title: "Evolving Import Style For Diff Friendliness"
date: 2020-03-17
layout: post
categories: programming
---

Raise your hand if you've been annoyed by imports in Haskell.

They're not fun.
Imports are often noisy, lists are often huge, and diffs can be truly nightmarish to compare.
Using a term often requires modifying the import list, which breaks your workflow.
Fortunately, we can reduce some of the pain of these problems with a few choices in our `stylish-haskell` configuration and a script that gradually implements these changes in your codebase.

This post begins with a style recommendation, continues with a script to implement it gradually in your codebase, and finishes with a discussion on relevant import styles and how they affect review quality.

# The Blessed Style

I use [`stylish-haskell`](https://hackage.haskell.org/package/stylish-haskell) for my formatting tool.
My editor's default formatting choices with [`vim2hs`](http://github.com/parsonsmatt/vim2hs) work well for me (while I maintain that fork, it's mostly a conglomeration of a bunch of changes that other people have made to it).

I have this shortcut defined to run `stylish-haskell` in vim:

```vimscript
" Haskell
nnoremap <leader>hs ms:%!stylish-haskell<cr>'s
```

This sets a mark, filters the file through `stylish-haskell`, and then returns to the mark.

`stylish-haskell` is configured by a `.stylish-haskell.yaml` file, and it will walk up the directory tree searching for one to configure the project with.
I place mine in the root of the Haskell directory, right next to the `stack.yaml` or `cabal.project` files.
Here are the contents that I recommend:

```yaml
steps:
  - imports:
      align: none
      list_align: with_module_name
      pad_module_names: false
      long_list_align: new_line_multiline
      empty_list_align: inherit
      list_padding: 7 # length "import "
      separate_lists: false
      space_surround: false
  - language_pragmas:
      style: vertical
      align: false
      remove_redundant: true
  - simple_align:
      cases: false
      top_level_patterns: false
      records: false 
  - trailing_whitespace: {}

# You need to put any language extensions that's enabled for the entire project here.
language_extensions: []

# This is up to personal preference, but 80 is the right answer.
columns: 80
```

Let's look at a diff that compares the default stylish-haskell and this configuration.
I created a [pull request](https://github.com/parsonsmatt/servant-persistent/pull/41) against the `servant-persistent` example project to demonstrate the style.
I left a bunch of review comments to explain the differences, and the UI for reading them is nice on GitHub.
Here's a reproduction of the differences:

```diff
- import           Init (runApp)
+ import Init (runApp)
```

We no longer indent so that module names are aligned. This helps keep the column count low, and makes it easier to just type this out manually without worrying about alignment.

```diff
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DataKinds #-}
```

We don't align on pragmas anymore. The diff will only show a new language pragma, not highlighting every line that was changed just to align the imports.

```diff
- import           Api.User             (UserAPI, userApi, userServer)
- import           Config               (AppT (..), Config (..))
+ import Api.User (UserAPI, userApi, userServer)
+ import Config (AppT(..), Config(..))
```

We no longer align the explicit import lists along the longest module name. This is less noisy, because adding a new module import that is longer than any others will no longer trigger a reformat across all the imports.

```diff
- import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
-                                               selectFirst, selectList, (==.))
+ import Database.Persist.Postgresql
+        (Entity(..), fromSqlKey, insert, selectFirst, selectList, (==.))
```

If the import and module goes beyond the column count, then the import list is indented, but is kept on one line. This keeps the import lists compact in the smallest cases, where it's easier to notice a small change.

```diff
- import           Servant              ((:<|>) ((:<|>)), Proxy (Proxy), Raw,
-                                        Server, serve, serveDirectoryFileServer)
+ import Servant
+        ( (:<|>)((:<|>))
+        , Proxy(Proxy)
+        , Raw
+        , Server
+        , serve
+        , serveDirectoryFileServer
+        )
```

If a newline indented import list expands beyond the column count, then it'll put each term on a new line. This takes up space, but it's really easy to read, and the diff for adding or removing an import line points to exactly the change that was made.

```diff
- import           Config                      (AppT (..))
- import           Control.Monad.Metrics       (increment, metricsCounters)
- import           Data.HashMap.Lazy           (HashMap)
- import           Data.IORef                  (readIORef)
- import           Data.Text                   (Text)
- import           Lens.Micro                  ((^.))
- import           Models                      (User (User), runDb, userEmail,
-                                               userName)
- import qualified Models                      as Md
- import qualified System.Metrics.Counter      as Counter
+ import Config (AppT(..))
+ import Control.Monad.Metrics (increment, metricsCounters)
+ import Data.HashMap.Lazy (HashMap)
+ import Data.IORef (readIORef)
+ import Data.Text (Text)
+ import Lens.Micro ((^.))
+ import Models (User(User), runDb, userEmail, userName)
+ import qualified Models as Md
+ import qualified System.Metrics.Counter as Counter
```

The end result is less pretty.
It's a little more cluttered to read.
However, it dramatically improves diffs and merge conflicts when using qualified and explicit imports, which will improve the overall readability of the codebase significantly.

# Automating the Migration

You don't want to shotgun the entire project with this, because that'll cause a nightmare of merge conflicts for everyone until the dust settles.
But if you did, you could write:

```sh
$ stylish-haskell --inplace **/.hs
```

This is fine for small projects with few collaborators.
But on large projects with many collaborators, we want to make this a bit more gentle.
So instead, we'll only require that files *changed* in a given PR are formatted.

We can get that information using `git diff --name-status origin/master`.
If your "target" remote and branch isn't `origin master` then substitute whatever you use.

The output of that command looks like this:

```
M       .stylish-haskell.yaml
M       Setup.hs
M       app/Main.hs
M       src/Api.hs
M       src/Api/User.hs
M       src/Config.hs
M       src/DevelMain.hs
M       src/Init.hs
M       src/Logger.hs
M       src/Models.hs
M       test/ApiSpec.hs
M       test/UserDbSpec.hs
```

All of these symbols are `M`, but you can also get `A` for additions and `R` for replacements/rewrites, and we'll want to `stylish` those up too.
We'll handle these in three steps for these cases, because it's easiest.

The first case is simply `M`, and we can focus on that with `grep "^M"`.
We only want Haskell files, so we'll filter on those with `grep ".hs"`.
We want to get the second field, so we'll do `cut -f 2`.
Finally, we'll send all the elements as arguments to `stylish-haskell --inplace` using `xargs`.
The whole command is here:

```
git diff --name-status origin/master \
  | grep .hs                         \
  | grep "^M"                        \
  | cut -f 2                         \
  | xargs stylish-haskell --inplace
```

Added files is the same, but you'll have `grep "^A"` instead.

Replaced/rewritten files are slightly different.
Those have three fields - the type (`R`), the original filename, and the destination/new file name.
We only want the new file name.
So the script looks like this:

```
# renamed files
git diff --name-status origin/master \
  | grep .hs                         \
  | grep "^R"                        \
  | cut -f 3                         \
  | xargs stylish-haskell --inplace
```

The only real difference is the `cut -f 3` field.

Our full script is:

```bash
#!/usr/bin/env bash

set -Eeux

# modified files
git diff --name-status origin/master \
  | grep .hs                         \
  | grep "^M"                        \
  | cut -f 2                         \
  | xargs stylish-haskell --inplace

# added files
git diff --name-status origin/master \
  | grep .hs                         \
  | grep "^A"                        \
  | cut -f 2                         \
  | xargs stylish-haskell --inplace

# renamed files
git diff --name-status origin/master \
  | grep .hs                         \
  | grep "^R"                        \
  | cut -f 3                         \
  | xargs stylish-haskell --inplace
```

Save that somewhere as `stylish-haskell.sh`, and add an entry in your `Makefile` that references it (you *do* have a Makefile, right?).

Now, we can run `make stylish` and it'll format all imports that have changed in our PR, but it won't touch anything else.
Over time, the codebase will converge on the new style, but only as people are working on relevant changes.

# Adding to CI

We can add this to CI by calling the script and seeing if anything changed.
`git` has an option `--exit-code` that will cause `git` to exit with a failure if there is a difference.
In this snippet, I have some uncommitted changes:

```
$ git diff --exit-code
diff --git a/Makefile b/Makefile
index f8d1636..df336de 100644
--- a/Makefile
+++ b/Makefile
@@ -6,4 +6,7 @@ ghcid-devel: ## Run the server in fast development mode. See DevelMain for detai
 	    --command "stack ghci servant-persistent" \
 	    --test "DevelMain.update"
 
-.PHONY: ghcid-devel help
+imports: ## Format all the imports that have changed since the master branch.
+	./stylish-haskell.sh
+
+.PHONY: ghcid-devel help imports

$ echo $?
1
```

We can use this to fail CI.
In Travis CI, we can add the following lines:

```yaml
script: 
- make imports
- git diff --exit-code
- stack --no-terminal --install-ghc test
```

You can adapt this to whatever CI setup you need.
However, you'll probably need to *install* `stylish-haskell` in CI, too.
Your build tool can handle that, just ensure that it's present on the `PATH`.

# Why this style?

The default style is really aesthetically nice.
Everything lines up, there's a lot of horizontal whitespace, it's uncluttered looking.
But it just doesn't scale!

It doesn't look good with long module names.
It doesn't look good with long explicit import lists.
It causes a ton of irrelevant diff noise and needless merge conflicts.
It becomes a hassle when you're working on a large codebase with other people.

So let's look at all the choices, their alternatives, and why I selected these.

```
steps:
  - imports:
      align: none
```

Alignment is visually appealing but it creates diff noise and it consumes columns with whitespace that would better be used with meaning.

```
      list_align: with_module_name
```

This option is superfluous, because we have selected `new_line_multiline` for `long_list_align`.

```
      pad_module_names: false
```

The docs for this give the justification quite nicely:

>  Right-pad the module names to align imports in a group:
> 
>  - true: a little more readable
> 
>    ```
>    > import qualified Data.List       as List (concat, foldl, foldr,
>    >                                           init, last, length)
>    > import qualified Data.List.Extra as List (concat, foldl, foldr,
>    >                                           init, last, length)
>    ```
> 
>  - false: diff-safe
> 
>    ```
>    > import qualified Data.List as List (concat, foldl, foldr, init,
>    >                                     last, length)
>    > import qualified Data.List.Extra as List (concat, foldl, foldr,
>    >                                           init, last, length)
>    ```
> 
>  Default: true

Ultimately, diff-safe is preferable to aesthetics, so we go with that.

```
      long_list_align: new_line_multiline
```

`long_list_align` determines what happens when the import list goes over the maximum column count.

This is option a recent addition to the options.
There are a few choices here, and you may actually prefer an even more diff-friendly approach than me.
`new_line_multiline` will indent if the module and list exceeds the column length.
If the new line list also exceeds the column length, then it'll put every import on it's own line.
This is fantastic for diffs, but takes up a lot of space.
It looks quite readable, at least.


```
      empty_list_align: inherit
```

This is a mostly irrelevant choice, since there is no alignment.

```
      list_padding: 7 # length "import "
```

This sets it up so that the import list clears the `import `, providing a clean visual break between lines.
You could go longer or shorter, but that's up to you.

```
      separate_lists: false
```

`separate_lists` adds a space between a class and it's methods or a type and it's constructors.

> - true: There is single space between Foldable type and list of it's
>   functions.
>
>   > import Data.Foldable (Foldable (fold, foldl, foldMap))
>
> - false: There is no space between Foldable type and list of it's
>   functions.
>
>   > import Data.Foldable (Foldable(fold, foldl, foldMap))

I like it off, but this can go either way.

```
      space_surround: false
```

This doesn't really matter and can go either way.
With `multiline` and now `new_line_multiline`, this is probably better to be `true`.

> Space surround option affects formatting of import lists on a single
> line. The only difference is single space after the initial
> parenthesis and a single space before the terminal parenthesis.
>
> - true: There is single space associated with the enclosing
>   parenthesis.
>
>   > import Data.Foo ( foo )
>
> - false: There is no space associated with the enclosing parenthesis
>
>   > import Data.Foo (foo)
>
> Default: false


```
  - language_pragmas:
      style: vertical
      align: false
      remove_redundant: true
```

I know it looks nice to have aligned pragmas, but it's annoying to view a diff and not easily tell what pragmas were added or removed.
THis makes it obvious.

```
  - simple_align:
     cases: false
     top_level_patterns: false
     records: false
```

All of this visual alignment just ruins diffs.
If you want visual alignment, align on an indentation boundary.
Compare:

```haskell
fromMaybe default maybeA = case maybeA of
                                Just a  -> a
                                Nothing -> default
```

This looks nice, but it's annoying to maintain and change.

```haskell
fromMaybe default maybeA = 
    case maybeA of
        Just a ->
            a
        Nothing ->
            default
```

You still get alignment of the important bits, but it's now safe to diffs and refactoring.

Likewise, adding, removing, or changing a field to a record should only trigger a diff on the relevant fields.
Anything else is noise that detracts from signal.

# Conclusion

Anyway, these are my recommendations for large projects that have multiple collaborators.
If you're working on a small project, then you don't need to worry about anything here.
These aren't my aesthetic preferneces, but these formatting choices do annoy me a lot less than pretty code pleases me.
