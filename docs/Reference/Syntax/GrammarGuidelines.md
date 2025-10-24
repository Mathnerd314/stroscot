VSCode Syntax Highlight Guide
=============================

Syntax highlighting determines the color and style of source code displayed in the Visual Studio Code editor. It is responsible for colorizing keywords like `if` or `for` in JavaScript differently than strings and comments and variable names.

There are two components to syntax highlighting:

-   [Tokenization](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#tokenization): Breaking text into a list of tokens
-   [Theming](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#theming): Using themes or user settings to map the tokens to specific colors and styles

Before diving into the details, a good start is to play with the [scope inspector](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#scope-inspector) tool and explore what tokens are present in a source file and what theme rules they match to. To see both semantic and syntax token, use a built-in theme (for example, Dark+) on a TypeScript file.

[Tokenization](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#tokenization)
---------------------------------------------------------------------------------------------------------

The tokenization of text is about breaking the text into segments and to classify each segment with a token type.

VS Code's tokenization engine is powered by [TextMate grammars](https://macromates.com/manual/en/language_grammars). TextMate grammars are a structured collection of regular expressions and are written as a plist (XML) or JSON files. VS Code extensions can contribute grammars through the `grammars` contribution point.

The TextMate tokenization engine runs in the same process as the renderer and tokens are updated as the user types. Tokens are used for syntax highlighting, but also to classify the source code into areas of comments, strings, regex.

Starting with release 1.43, VS Code also allows extensions to provide tokenization through a [Semantic Token Provider](https://code.visualstudio.com/api/references/vscode-api#DocumentSemanticTokensProvider). Semantic providers are typically implemented by language servers that have a deeper understanding of the source file and can resolve symbols in the context of the project. For example, a constant variable name can be rendered using constant highlighting throughout the project, not just at the place of its declaration.

Highlighting based on semantic tokens is considered an addition to the TextMate-based syntax highlighting. Semantic highlighting goes on top of the syntax highlighting. And as language servers can take a while to load and analyze a project, semantic token highlighting may appear after a short delay.

This article focuses on the TextMate-based tokenization. Semantic tokenization and theming are explained in the [Semantic Highlighting guide](https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide).

### [TextMate grammars](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#textmate-grammars)

VS Code uses [TextMate grammars](https://macromates.com/manual/en/language_grammars) as the syntax tokenization engine. Invented for the TextMate editor, they have been adopted by many other editors and IDEs due to large number of language bundles created and maintained by the Open Source community.

TextMate grammars rely on [Oniguruma regular expressions](https://macromates.com/manual/en/regular_expressions) and are typically written as a plist or JSON. You can find a good introduction to TextMate grammars [here](https://www.apeth.com/nonblog/stories/textmatebundle.html), and you can take a look at existing TextMate grammars to learn more about how they work.

### [TextMate tokens and scopes](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#textmate-tokens-and-scopes)

Tokens are one or more characters that are part of the same program element. Example tokens include operators such as `+` and `*`, variable names such as `myVar`, or strings such as `"my string"`.

Each token is associated with a scope that defines the context of the token. A scope is a dot separated list of identifiers that specify the context of the current token. The `+` operation in JavaScript, for example, has the scope `keyword.operator.arithmetic.js`.

Themes map scopes to colors and styles to provide syntax highlighting. TextMate provides [list of common scopes](https://macromates.com/manual/en/language_grammars) that many themes target. In order to have your grammar as broadly supported as possible, try to build on existing scopes rather than defining new ones.

Scopes nest so that each token is also associated with a list of parent scopes. The example below uses the [scope inspector](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#scope-inspector) to show the scope hierarchy for the `+` operator in a simple JavaScript function. The most specific scope is listed at the top, with more general parent scopes listed below:

![syntax highlighting scopes](https://code.visualstudio.com/assets/api/language-extensions/syntax-highlighting/scopes.png)

Parent scope information is also used for theming. When a theme targets a scope, all tokens with that parent scope will be colorized unless the theme also provides a more specific colorization for their individual scopes.

### [Contributing a basic grammar](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#contributing-a-basic-grammar)

VS Code supports json TextMate grammars. These are contributed through the `grammars` [contribution point](https://code.visualstudio.com/api/references/contribution-points).

Each grammar contribution specifies: the identifier of the language the grammar applies to, the top-level scope name for the tokens of the grammar, and the relative path to a grammar file. The example below shows a grammar contribution for a fictional `abc` language:

```
{
  "contributes": {
    "languages": [
      {
        "id": "abc",
        "extensions": [".abc"]
      }
    ],
    "grammars": [
      {
        "language": "abc",
        "scopeName": "source.abc",
        "path": "./syntaxes/abc.tmGrammar.json"
      }
    ]
  }
}

```

The grammar file itself consists of a top-level rule. This is typically split into a `patterns` section that lists the top-level elements of the program and a `repository` that defines each of the elements. Other rules in the grammar can reference elements from the `repository` using `{ "include": "#id" }`.

The example `abc` grammar marks the letters `a`, `b`, and `c` as keywords, and nestings of parens as expressions.

```
{
  "scopeName": "source.abc",
  "patterns": [{ "include": "#expression" }],
  "repository": {
    "expression": {
      "patterns": [{ "include": "#letter" }, { "include": "#paren-expression" }]
    },
    "letter": {
      "match": "a|b|c",
      "name": "keyword.letter"
    },
    "paren-expression": {
      "begin": "\\(",
      "end": "\\)",
      "beginCaptures": {
        "0": { "name": "punctuation.paren.open" }
      },
      "endCaptures": {
        "0": { "name": "punctuation.paren.close" }
      },
      "name": "expression.group",
      "patterns": [{ "include": "#expression" }]
    }
  }
}

```

The grammar engine will try to successively apply the `expression` rule to all text in the document. For a simple program such as:

```
a
(
    b
)
x
(
    (
        c
        xyz
    )
)
(
a

```

The example grammar produces the following scopes (listed left-to-right from most specific to least specific scope):

```
a               keyword.letter, source.abc
(               punctuation.paren.open, expression.group, source.abc
    b           keyword.letter, expression.group, source.abc
)               punctuation.paren.close, expression.group, source.abc
x               source.abc
(               punctuation.paren.open, expression.group, source.abc
    (           punctuation.paren.open, expression.group, expression.group, source.abc
        c       keyword.letter, expression.group, expression.group, source.abc
        xyz     expression.group, expression.group, source.abc
    )           punctuation.paren.close, expression.group, expression.group, source.abc
)               punctuation.paren.close, expression.group, source.abc
(               punctuation.paren.open, expression.group, source.abc
a               keyword.letter, expression.group, source.abc

```

Note that text that is not matched by one of the rules, such as the string `xyz`, is included in the current scope. The last parenthesis at the end of the file is part of the `expression.group` even if the `end` rule is not matched, as `end-of-document` was found before the `end` rule was.

### [Embedded languages](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#embedded-languages)

If your grammar includes embedded languages within the parent language, such as CSS style blocks in HTML, you can use the `embeddedLanguages` contribution point to tell VS Code to treat the embedded language as distinct from the parent language. This ensures that bracket matching, commenting, and other basic language features work as expected in the embedded language.

The `embeddedLanguages` contribution point maps a scope in the embedded language to a top-level language scope. In the example below, any tokens in the `meta.embedded.block.javascript` scope will be treated as JavaScript content:

```
{
  "contributes": {
    "grammars": [
      {
        "path": "./syntaxes/abc.tmLanguage.json",
        "scopeName": "source.abc",
        "embeddedLanguages": {
          "meta.embedded.block.javascript": "javascript"
        }
      }
    ]
  }
}

```

Now if you try to comment code or trigger snippets inside a set of tokens marked `meta.embedded.block.javascript`, they will get the correct `//` JavaScript style comment and the correct JavaScript snippets.

### [Developing a new grammar extension](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#developing-a-new-grammar-extension)

To quickly create a new grammar extension, use [VS Code's Yeoman templates](https://code.visualstudio.com/api/get-started/your-first-extension) to run `yo code` and select the `New Language` option:

![Selecting the 'new language' template in 'yo code'](https://code.visualstudio.com/assets/api/language-extensions/syntax-highlighting/yo-new-language.png)

Yeoman will walk you through some basic questions to scaffold the new extension. The important questions for creating a new grammar are:

-   `Language id` - A unique identifier for your language.
-   `Language name` - A human readable name for your language.
-   `Scope names` - Root TextMate scope name for your grammar.

![Filling in the 'new language' questions](https://code.visualstudio.com/assets/api/language-extensions/syntax-highlighting/yo-new-language-questions.png)

The generator assumes that you want to define both a new language and a new grammar for that language. If you are creating a grammar for an existing language, just fill these in with your target language's information and be sure to delete the `languages` contribution point in the generated `package.json`.

After answering all the questions, Yeoman will create a new extension with the structure:

![A new language extension](https://code.visualstudio.com/assets/api/language-extensions/syntax-highlighting/generated-new-language-extension.png)

Remember, if you are contributing a grammar to a language that VS Code already knows about, be sure to delete the `languages` contribution point in the generated `package.json`.

#### Converting an existing TextMate grammar

`yo code` can also help convert an existing TextMate grammar to a VS Code extension. Again, start by running `yo code` and selecting `Language extension`. When asked for an existing grammar file, give it the full path to either a `.tmLanguage` or `.json` TextMate grammar file:

![Converting an existing TextMate grammar](https://code.visualstudio.com/assets/api/language-extensions/syntax-highlighting/yo-convert.png)

#### Using YAML to write a grammar

As a grammar grows more complex, it can become difficult to understand and maintain it as json. If you find yourself writing complex regular expressions or needing to add comments to explain aspects of the grammar, consider using yaml to define your grammar instead.

Yaml grammars have the exact same structure as a json based grammar but allow you to use yaml's more concise syntax, along with features such as multi-line strings and comments.

![A yaml grammar using multiline strings and comments](https://code.visualstudio.com/assets/api/language-extensions/syntax-highlighting/yaml-grammar.png)

VS Code can only load json grammars, so yaml based grammars must be converted to json. The [`js-yaml` package](https://www.npmjs.com/package/js-yaml) and command-line tool makes this easy.

```
# Install js-yaml as a development only dependency in your extension
$ npm install js-yaml --save-dev

# Use the command-line tool to convert the yaml grammar to json
$ npx js-yaml syntaxes/abc.tmLanguage.yaml > syntaxes/abc.tmLanguage.json

```

### [Injection grammars](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#injection-grammars)

Injection grammars let you extend an existing grammar. An injection grammar is a regular TextMate grammar that is injected into a specific scope within an existing grammar. Example applications of injection grammars:

-   Highlighting keywords such as `TODO` in comments.
-   Add more specific scope information to an existing grammar.
-   Adding highlighting for a new language to Markdown fenced code blocks.

#### Creating a basic injection grammar

Injection grammars are contributed though the `package.json` just like regular grammars. However, instead of specifying a `language`, an injection grammar uses `injectTo` to specify a list of target language scopes to inject the grammar into.

For this example, we'll create a simple injection grammar that highlights `TODO` as a keyword in JavaScript comments. To apply our injection grammar in JavaScript files, we use the `source.js` target language scope in `injectTo`:

```
{
  "contributes": {
    "grammars": [
      {
        "path": "./syntaxes/injection.json",
        "scopeName": "todo-comment.injection",
        "injectTo": ["source.js"]
      }
    ]
  }
}

```

The grammar itself is a standard TextMate grammar except for the top level `injectionSelector` entry. The `injectionSelector` is a scope selector that specifies which scopes the injected grammar should be applied in. For our example, we want to highlight the word `TODO` in all `//` comments. Using the [scope inspector](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#scope-inspector), we find that JavaScript's double slash comments have the scope `comment.line.double-slash`, so our injection selector is `L:comment.line.double-slash`:

```
{
  "scopeName": "todo-comment.injection",
  "injectionSelector": "L:comment.line.double-slash",
  "patterns": [
    {
      "include": "#todo-keyword"
    }
  ],
  "repository": {
    "todo-keyword": {
      "match": "TODO",
      "name": "keyword.todo"
    }
  }
}

```

The `L:` in the injection selector means that the injection is added to the left of existing grammar rules. This basically means that our injected grammar's rules will be applied before any existing grammar rules.

#### Embedded languages

Injection grammars can also contribute embedded languages to their parent grammar. Just like with a normal grammar, an injection grammar can use `embeddedLanguages` to map scopes from the embedded language to a top-level language scope.

An extension that highlights SQL queries in JavaScript strings, for example, may use `embeddedLanguages` to make sure all token inside the string marked `meta.embedded.inline.sql` are treated as SQL for basic language features such as bracket matching and snippet selection.

```
{
  "contributes": {
    "grammars": [
      {
        "path": "./syntaxes/injection.json",
        "scopeName": "sql-string.injection",
        "injectTo": ["source.js"],
        "embeddedLanguages": {
          "meta.embedded.inline.sql": "sql"
        }
      }
    ]
  }
}

```

#### Token types and embedded languages

There is one additional complication for injection languages embedded languages: by default, VS Code treats all tokens within a string as string contents and all tokens with a comment as token content. Since features such as bracket matching and auto closing pairs are disabled inside of strings and comments, if the embedded language appears inside a string or comment, these features will also be disabled in the embedded language.

To override this behavior, you can use a `meta.embedded.*` scope to reset VS Code's marking of tokens as string or comment content. It is a good idea to always wrap embedded language in a `meta.embedded.*` scope to make sure VS Code treats the embedded language properly.

If you can't add a `meta.embedded.*` scope to your grammar, you can alternatively use `tokenTypes` in the grammar's contribution point to map specific scopes to content mode. The `tokenTypes` section below ensures that any content in the `my.sql.template.string` scope is treated as source code:

```
{
  "contributes": {
    "grammars": [
      {
        "path": "./syntaxes/injection.json",
        "scopeName": "sql-string.injection",
        "injectTo": ["source.js"],
        "embeddedLanguages": {
          "my.sql.template.string": "sql"
        },
        "tokenTypes": {
          "my.sql.template.string": "other"
        }
      }
    ]
  }
}

```

[Theming](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#theming)
-----------------------------------------------------------------------------------------------

Theming is about assigning colors and styles to tokens. Theming rules are specified in color themes, but users can customize the theming rules in the user settings.

TextMate theme rules are defined in `tokenColors` and have the same syntax as regular TextMate themes. Each rule defines a TextMate scope selector and a resulting color and style.

When evaluating the color and style of a token, the current token's scope is matched against the rule's selector to find the most specific rule for each style property (foreground, bold, italic, underline)

The [Color Theme guide](https://code.visualstudio.com/api/extension-guides/color-theme#syntax-colors) describes how to create a color theme. Theming for semantic tokens is explained in the [Semantic Highlighting guide](https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#theming).

[Scope inspector](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide#scope-inspector)
---------------------------------------------------------------------------------------------------------------

VS Code's built-in scope inspector tool helps debug grammars and semantic tokens. It displays the scopes for the token and the semantic tokens at the current position in a file, along with metadata about which theme rules apply to that token.

Trigger the scope inspector from the Command Palette with the `Developer: Inspect Editor Tokens and Scopes` command or [create a keybinding](https://code.visualstudio.com/docs/getstarted/keybindings) for it:

```
{
  "key": "cmd+alt+shift+i",
  "command": "editor.action.inspectTMScopes"
}

```

![scope inspector](https://code.visualstudio.com/assets/api/language-extensions/syntax-highlighting/scope-inspector.png)

The scope inspector displays the following information:

1.  The current token.
2.  Metadata about the token and information about its computed appearance. If you are working with embedded languages, the important entries here `language` and `token type`.
3.  The semantic token section is shown when a semantic token provider is available for the current language and when the current theme supports semantic highlighting. It shows the current semantic token type and modifiers along with the theme rules that match the semantic token type and modifiers.
4.  The TextMate section shows the scope list for the current TextMate token, with the most specific scope at the top. It also shows the most specific theme rules that match the scopes. This only shows the theme rules that are responsible for the token's current style, it does not show overridden rules. If semantic tokens are present, the theme rules are only shown when they differ from the rule matching the semantic token.

TextMate Language Grammars
==========================

Language grammars are used to assign names to document elements such as keywords, comments, strings or similar. The purpose of this is to allow styling (syntax highlighting) and to make the text editor "smart" about which context the caret is in. For example you may want a key stroke or tab trigger to act differently depending on the context, or you may want to disable spell check as you type those portions of your text document which are not prose (e.g. HTML tags).

The language grammar is used only to parse the document and assign names to subsets of this document. Then [scope selectors](https://macromates.com/manual/en/scope_selectors) can be used for styling, preferences and deciding how keys and tab triggers should expand.

For a more thorough introduction to this concept see the [introduction to scopes](https://macromates.com/blog/2005/introduction-to-scopes/) blog post.

Example Grammar
---------------

You can create a new language grammar by opening the bundle editor (Window → Show Bundle Editor) and select "New Language" from the add button in the lower left corner.

This will give you a starting grammar which will look like the one below, so let us start by explaining that.

```
 1  {  scopeName = 'source.untitled';
 2     fileTypes = ( );
 3     foldingStartMarker = '\{\s*$';
 4     foldingStopMarker = '^\s*\}';
 5     patterns = (
 6        {  name = 'keyword.control.untitled';
 7           match = '\b(if|while|for|return)\b';
 8        },
 9        {  name = 'string.quoted.double.untitled';
10           begin = '"';
11           end = '"';
12           patterns = (
13              {  name = 'constant.character.escape.untitled';
14                 match = '\\.';
15              }
16           );
17        },
18     );
19  }

```

The format is the [property list format](https://macromates.com/manual/en/appendix#property-list-format) and at the root level there are five key/value pairs:

-   `scopeName` (line 1) --- this should be a unique name for the grammar, following the convention of being a dot-separated name where each new (left-most) part specializes the name. Normally it would be a two-part name where the first is either `text` or `source` and the second is the name of the language or document type. But if you are specializing an existing type, you probably want to derive the name from the type you are specializing. For example Markdown is `text.html.markdown` and Ruby on Rails (`rhtml` files) is `text.html.rails`. The advantage of deriving it from (in this case) `text.html` is that everything which works in the `text.html` scope will also work in the `text.html.«something»` scope (but with a lower precedence than something specifically targeting `text.html.«something»`).

-   `fileTypes` (line 2) --- this is an array of file type extensions that the grammar should (by default) be used with. This is referenced when TextMate does not know what grammar to use for a file the user opens. If however the user selects a grammar from the language pop-up in the status bar, TextMate will remember that choice.

-   `foldingStartMarker` / `foldingStopMarker` (line 3-4) --- these are regular expressions that lines (in the document) are matched against. If a line matches one of the patterns (but not both), it becomes a folding marker (see the [foldings](https://macromates.com/manual/en/navigation_overview#customizing-foldings) section for more info).

-   `patterns` (line 5-18) --- this is an array with the actual rules used to parse the document. In this example there are two rules (line 6-8 and 9-17). Rules will be explained in the next section.

There are two additional (root level) keys which are not used in the example:

-   `firstLineMatch` --- a regular expression which is matched against the first line of the document (when it is first loaded). If it matches, the grammar is used for the document (unless there is a user override). Example: `^#!/.*\bruby\b`.

-   `repository` --- a dictionary (i.e. key/value pairs) of rules which can be included from other places in the grammar. The key is the name of the rule and the value is the actual rule. Further explanation (and example) follow with the description of the `include` rule key.

Language Rules
--------------

A language rule is responsible for matching a portion of the document. Generally a rule will specify a name which gets assigned to the part of the document which is matched by that rule.

There are two ways a rule can match the document. It can either provide a single regular expression, or two. As with the `match` key in the first rule above (lines 6-8), everything which matches that regular expression will then get the name specified by that rule. For example the first rule above assigns the name `keyword.control.untitled` to the following keywords: `if`, `while`, `for` and `return`. We can then use a [scope selector](https://macromates.com/manual/en/scope_selectors) of `keyword.control` to have our [theme](https://macromates.com/manual/en/themes) style these keywords.

The other type of match is the one used by the second rule (lines 9-17). Here two regular expressions are given using the `begin` and `end` keys. The name of the rule will be assigned from where the begin pattern matches to where the end pattern matches (including both matches). If there is no match for the end pattern, the end of the document is used.

In this latter form, the rule can have sub-rules which are matched against the part between the begin and end matches. In our example here we match strings that start and end with a quote character and escape characters are marked up as `constant.character.escape.untitled` inside the matched strings (line 13-15).

*Note that the regular expressions are matched against only a **single line of the document** at a time. That means it is **not possible to use a pattern that matches multiple lines**.* The reason for this is technical: being able to restart the parser at an arbitrary line and having to re-parse only the minimal number of lines affected by an edit. In most situations it is possible to use the begin/end model to overcome this limitation.

Rule Keys
---------

What follows is a list of all keys which can be used in a rule.

-   `name` --- the name which gets assigned to the portion matched. This is used for styling and scope-specific settings and actions, which means it should generally be derived from one of the standard names (see [naming conventions](https://macromates.com/manual/en/language_grammars#naming-conventions) later).

-   `match` --- a regular expression which is used to identify the portion of text to which the name should be assigned. Example: `'\b(true|false)\b'`.

-   `begin`, `end` --- these keys allow matches which span several lines and must both be mutually exclusive with the `match` key. Each is a regular expression pattern. `begin` is the pattern that starts the block and `end` is the pattern which ends the block. Captures from the `begin` pattern can be referenced in the `end` pattern by using normal regular expression back-references. This is often used with here-docs, for example:

    ```
     {   name = 'string.unquoted.here-doc';
         begin = '<<(\w+)';  // match here-doc token
         end = '^\1$';       // match end of here-doc
     }

    ```

    A `begin`/`end` rule can have nested patterns using the `patterns` key. For example we can do:

    ```
     {  begin = '<%'; end = '%>'; patterns = (
           { match = '\b(def|end)\b'; ... },
           ...
        );
     };

    ```

    The above will match `def` and `end` keywords inside a `<% ... %>` block (though for embedded languages see info about the `include` key later).

-   `contentName` --- this key is similar to the `name` key but only assigns the name to the text **between** what is matched by the `begin`/`end` patterns. For example to get the text between `#if 0` and `#endif` marked up as a comment, we would do:

    ```
     {  begin = '#if 0(\s.*)?$'; end = '#endif';
        contentName = 'comment.block.preprocessor';
     };

    ```

-   `captures`, `beginCaptures`, `endCaptures` --- these keys allow you to assign attributes to the captures of the `match`, `begin`, or `end` patterns. Using the `captures` key for a `begin`/`end` rule is short-hand for giving both `beginCaptures` and `endCaptures` with same values.

    The value of these keys is a dictionary with the key being the capture number and the value being a dictionary of attributes to assign to the captured text. Currently `name` is the only attribute supported. Here is an example:

    ```
     {  match = '(@selector\()(.*?)(\))';
        captures = {
           1 = { name = 'storage.type.objc'; };
           3 = { name = 'storage.type.objc'; };
        };
     };

    ```

    In that example we match text like `@selector(windowWillClose:)` but the `storage.type.objc` name will only be assigned to `@selector(` and `)`.

-   `include` --- this allows you to reference a different language, recursively reference the grammar itself or a rule declared in this file's repository.

    1.  To reference another language, use the scope name of that language:

        ```
         {  begin = '<\?(php|=)?'; end = '\?>'; patterns = (
               { include = "source.php"; }
            );
         }

        ```

    2.  To reference the grammar itself, use `$self`:

        ```
         {  begin = '\('; end = '\)'; patterns = (
               { include = "$self"; }
            );
         }

        ```

    3.  To reference a rule from the current grammars repository, prefix the name with a pound sign (`#`):

        ```
         patterns = (
            {  begin = '"'; end = '"'; patterns = (
                  { include = "#escaped-char"; },
                  { include = "#variable"; }
               );
            },
            ...
         ); // end of patterns
         repository = {
            escaped-char = { match = '\\.'; };
            variable =     { match = '\$[a-zA-Z0-9_]+'; };
         };

        ```

        This can also be used to match recursive constructs like balanced characters:

        ```
         patterns = (
            {  name = 'string.unquoted.qq.perl';
               begin = 'qq\('; end = '\)'; patterns = (
                  { include = '#qq_string_content'; },
               );
            },
            ...
         ); // end of patterns
         repository = {
            qq_string_content = {
               begin = '\('; end = '\)'; patterns = (
                  { include = '#qq_string_content'; },
               );
            };
         };

        ```

        This will correctly match a string like: `qq( this (is (the) entire) string)`.

Naming Conventions
------------------

TextMate is free-form in the sense that you can assign basically any name you wish to any part of the document that you can markup with the grammar system and then use that name in [scope selectors](https://macromates.com/manual/en/scope_selectors).

There are however conventions so that one [theme](https://macromates.com/manual/en/themes) can target as many languages as possible, without having dozens of rules specific to each language and also so that functionality (mainly [preferences](https://macromates.com/manual/en/preferences_items)) can be re-used across languages, e.g. you probably do not want an apostrophe to be auto-paired when inserted in strings and comments, regardless of the language you are in, so it makes sense to only set this up once.

Before going through the conventions, here are a few things to keep in mind:

1.  A minimal theme will only assign styles to 10 of the 11 root groups below (`meta` does not get a visual style), so you should "spread out" your naming i.e. instead of putting everything below `keyword` (as your formal language definition may insist) you should think "would I want these two elements styled differently?" and if so, they should probably be put into different root groups.

2.  Even though you should "spread out" your names, when you have found the group in which you want to place your element (e.g. `storage`) you should re-use the existing names used below that group (for `storage` that is `modifier` or `type`) rather than make up a new sub-type. You should however append as much information to the sub-type you choose. For example if you are matching the `static` storage modifier, then instead of just naming it `storage.modifier` use `storage.modifier.static.«language»`. A scope selector of just `storage.modifier` will match both, but having the extra information in the name means it is possible to specifically target it disregarding the other storage modifiers.

3.  Put the language name last in the name. This may seem redundant, since you can generally use a scope selector of: `source.«language» storage.modifier`, but when embedding languages, this is not always possible.

And now the 11 root groups which are currently in use with some explanation about their intended purpose. This is presented as a hierarchical list but the actual scope name is obtained by joining the name from each level with a dot. For example `double-slash` is `comment.line.double-slash`.

-   `comment` --- for comments.

    -   `line` --- line comments, we specialize further so that the type of comment start character(s) can be extracted from the scope.
        -   `double-slash` --- `// comment`
        -   `double-dash` --- `-- comment`
        -   `number-sign` --- `# comment`
        -   `percentage` --- `% comment`
        -   *character* --- other types of line comments.
    -   `block` --- multi-line comments like `/* ... */` and `<!-- ... -->`.
        -   `documentation` --- embedded documentation.
-   `constant` --- various forms of constants.

    -   `numeric` --- those which represent numbers, e.g. `42`, `1.3f`, `0x4AB1U`.
    -   `character` --- those which represent characters, e.g. `&lt;`, `\e`, `\031`.
        -   `escape` --- escape sequences like `\e` would be `constant.character.escape`.
    -   `language` --- constants (generally) provided by the language which are "special" like `true`, `false`, `nil`, `YES`, `NO`, etc.
    -   `other` --- other constants, e.g. colors in CSS.
-   `entity` --- an entity refers to a larger part of the document, for example a chapter, class, function, or tag. We do not scope the entire entity as `entity.*` (we use `meta.*` for that). But we do use `entity.*` for the "placeholders" in the larger entity, e.g. if the entity is a chapter, we would use `entity.name.section` for the chapter title.

    -   `name` --- we are naming the larger entity.
        -   `function` --- the name of a function.
        -   `type` --- the name of a type declaration or class.
        -   `tag` --- a tag name.
        -   `section` --- the name is the name of a section/heading.
    -   `other` --- other entities.
        -   `inherited-class` --- the superclass/baseclass name.
        -   `attribute-name` --- the name of an attribute (mainly in tags).
-   `invalid` --- stuff which is "invalid".
    -   `illegal` --- illegal, e.g. an ampersand or lower-than character in HTML (which is not part of an entity/tag).
    -   `deprecated` --- for deprecated stuff e.g. using an API function which is deprecated or using styling with strict HTML.
-   `keyword` --- keywords (when these do not fall into the other groups).
    -   `control` --- mainly related to flow control like `continue`, `while`, `return`, etc.
    -   `operator` --- operators can either be textual (e.g. `or`) or be characters.
    -   `other` --- other keywords.
-   `markup` --- this is for markup languages and generally applies to larger subsets of the text.
    -   `underline` --- underlined text.
        -   `link` --- this is for links, as a convenience this is derived from `markup.underline` so that if there is no theme rule which specifically targets `markup.underline.link` then it will inherit the underline style.
    -   `bold` --- bold text (text which is strong and similar should preferably be derived from this name).
    -   `heading` --- a section header. Optionally provide the heading level as the next element, for example `markup.heading.2.html` for `<h2>...</h2>` in HTML.
    -   `italic` --- italic text (text which is emphasized and similar should preferably be derived from this name).
    -   `list` --- list items.
        -   `numbered` --- numbered list items.
        -   `unnumbered` --- unnumbered list items.
    -   `quote` --- quoted (sometimes block quoted) text.
    -   `raw` --- text which is verbatim, e.g. code listings. Normally spell checking is disabled for `markup.raw`.
    -   `other` --- other markup constructs.
-   `meta` --- the meta scope is generally used to markup larger parts of the document. For example the entire line which declares a function would be `meta.function` and the subsets would be `storage.type`, `entity.name.function`, `variable.parameter` etc. and only the latter would be styled. Sometimes the meta part of the scope will be used only to limit the more general element that is styled, most of the time meta scopes are however used in scope selectors for activation of bundle items. For example in Objective-C there is a meta scope for the interface declaration of a class and the implementation, allowing the same tab-triggers to expand differently, depending on context.

-   `storage` --- things relating to "storage".
    -   `type` --- the type of something, `class`, `function`, `int`, `var`, etc.
    -   `modifier` --- a storage modifier like `static`, `final`, `abstract`, etc.
-   `string` --- strings.
    -   `quoted` --- quoted strings.
        -   `single` --- single quoted strings: `'foo'`.
        -   `double` --- double quoted strings: `"foo"`.
        -   `triple` --- triple quoted strings: `"""Python"""`.
        -   `other` --- other types of quoting: `$'shell'`, `%s{...}`.
    -   `unquoted` --- for things like here-docs and here-strings.
    -   `interpolated` --- strings which are "evaluated": ``date``, `$(pwd)`.
    -   `regexp` --- regular expressions: `/(\w+)/`.
    -   `other` --- other types of strings (should rarely be used).
-   `support` --- things provided by a framework or library should be below `support`.
    -   `function` --- functions provided by the framework/library. For example `NSLog` in Objective-C is `support.function`.
    -   `class` --- when the framework/library provides classes.
    -   `type` --- types provided by the framework/library, this is probably only used for languages derived from C, which has `typedef` (and `struct`). Most other languages would introduce new types as classes.
    -   `constant` --- constants (magic values) provided by the framework/library.
    -   `variable` --- variables provided by the framework/library. For example `NSApp` in AppKit.
    -   `other` --- the above should be exhaustive, but for everything else use `support.other`.
-   `variable` --- variables. Not all languages allow easy identification (and thus markup) of these.
    -   `parameter` --- when the variable is declared as the parameter.
    -   `language` --- reserved language variables like `this`, `super`, `self`, etc.
    -   `other` --- other variables, like `$some_variables`.


Writing a TextMate Grammar: Some Lessons Learned
================================================

### February 17, 2014 at 14:35:46

Although TextMate has been around for a long time (in computer years) and many language bundles exist, it is startling to find that the process of writing a language grammar remains poorly documented. Having recently managed to write a [grammar of my own](https://github.com/mattneub/AsciiDoc-TextMate-2) for the first time, here are some things I learned along the way.

Time Involved
-------------

Writing a grammar can be a slow business. It took me some weeks just to prepare, collecting information and locating and studying the existing instructions and documentation.

It then took me about three weeks of extremely frustrating, difficult work before I got my *first two scopes* working in the grammar. (This was mostly because of undocumented things *you* mustn't do and things that TextMate *can't* do, both of which I tell you about later on this page.) After that, though, it was remarkably smooth sailing, and I was able to finish the entire grammar in a week. Here are some git log excerpts:

```
Date:   Wed Jan 15 13:59:57 2014 -0800
    initial commit

Date:   Wed Feb 5 09:20:18 2014 -0800
    finally got recursively related match patterns working!

Date:   Wed Feb 12 19:58:51 2014 -0800
    ready for first release!

```

How Scopes Are Styled
---------------------

One of the chief reasons for writing a grammar is syntax coloring --- which, like so much else about a grammar, depends on scopes. The rules for how a scope should be colored and styled, however, are to be found, not in the grammar itself, but in *themes* and also in *settings*.

In particular:

-   In TextMate 2, the standard themes are all in the Themes bundle. The user will probably have selected one of these themes. You have no control over which it is; yet upon this choice depends how your grammar will affect the look of a document.

-   Themes can also be hidden away in other bundles. (Fortunately, at the moment, no standard bundle that I use, other than the Themes bundle, contains any themes.)

-   Settings can color and style scopes, like miniature one-rule themes.

Since themes and settings are going to have an effect on how your scopes are displayed, you should take some time to examine the existing themes, as well as looking through all settings of all bundles, to get some notion of what this effect might be. To save you some time, I will now summarize the information that I gathered in this regard.

### Standard Themes

Unfortunately, different standard themes affect different scopes; ideally you should note down *every* scope listed in *every* theme, but this would take too long. Here are some of the *main general scopes* that are styled by the standard themes; these are scopes that you generally *want* to use, because you can rely on the user to have selected a standard theme that will style them for you:

```
comment
constant
constant.character.escape
constant.language
constant.numeric
declaration.section entity.name.section
declaration.tag
deco.folding
entity.name.function
entity.name.tag
entity.name.type
entity.other.attribute-name
entity.other.inherited-class
invalid
invalid.deprecated.trailing-whitespace
keyword
keyword.control.import
keyword.operator.js
markup.heading
markup.list
markup.quote
meta.embedded
meta.preprocessor
meta.section entity.name.section
meta.tag
storage
storage.type.method
string
string source
string.unquoted
support.class
support.constant
support.function
support.type
support.variable
text source
variable
variable.language
variable.other
variable.parameter

```

### Settings

I was stunned to discover that, in addition to themes, a bundle's settings can style scopes. It turns out that this fact is extremely important. For example, I scratched my head for about two weeks, trying to figure out why my italic text was *shown* as italic. I couldn't find any theme that was doing this. Then one day I stumbled on the Text bundle, which contains the responsible setting.

Here are the chief bundle settings to be aware of:

-   **Text bundle**: `markup.bold`, `markup.italic`, `markup.underline`. (The underlining imposed by `markup.underline` is also how the Hyperlink Helper bundle injects underlining onto your URLs.) There are some other settings here, but they affect things like soft wrapping (also something to watch out for).

-   **Themes bundle**: `markup.heading.n`, `markup.quote`, `markup.raw.block`. These settings affect both the font and size of these scopes. In my opinion, they are among the most horrible side effects of using TextMate 2. (If I wanted font and size to vary through my document, I'd be using Microsoft Word, for heaven's sake. I'm here for the *text*. It's called *TextMate*, after all.) To avoid having your text affected by these settings, *avoid these scopes*. I have also disabled these settings in my copy of TextMate, but you can't rely on every user to do that.

-   **TextMate bundle**: `meta.separator`.

If you use or enable other bundles, keep your eyes peeled for settings that may perform scope styling. For example:

-   **Diff bundle**: `markup.changed`, `markup.deleted`, `markup.inserted`; `meta.diff.header`, `meta.diff.index`, `meta.diff.range`.

The Source bundle doesn't contain any style settings, but it does have some settings that determine what counts as a word character for purposes of code completion (as explained in [this article](http://blog.macromates.com/2012/clever-completion/)) as well as word selection and navigation, so you might want to be careful with the distinctions being made here.

Standard Scopes
---------------

The official list of scopes that are considered standard or conventional is not quite the same as the list of scopes that are commonly styled by themes. It's good to have this list on hand. The list as given in the [documentation](http://manual.macromates.com/en/language_grammars#naming_conventions) is as follows. I have starred once the entries that are present also in the list of commonly styled scopes (though note that for some reason there are commonly styled scopes that are not present in this list); I have double-starred the entries that are styled by settings:

```
comment*
    line
        double-slash
        double-dash
        number-sign
        percentage
        [character]
    block
        documentation
constant*
    numeric*
    character
        escape*
    language*
    other
entity
    name
        function*
        type*
        tag()
        section*
    other
        inherited-class*
        attribute-name*
invalid*
    illegal
    deprecated
keyword*
    control [control.import*]
    operator [operator.js*]
    other
markup
    underline**
        link
    bold**
    heading*
    italic**
    list*
        numbered
        unnumbered
    quote* (and **)
    raw
    other
meta
storage*
    type [type.method*]
    modifier
string* (and string source*)
    quoted
        single
        double
        triple
        other
    unquoted*
    interpolated
    regexp
    other
support
    function*
    class*
    type*
    constant*
    variable*
    other [other.variable*]
variable
    parameter*
    language*
    other*

```

The Bundle Development bundle will also assist you as you edit your grammar by marking unusual scope names.

Note that although the documentation claims that `meta` scope is not styled, nevertheless some `meta` subscopes are in the list of scopes styled by themes and settings above.

Regular Expressions
-------------------

Grammar match rules are written as Ruby regular expressions. You need to be conversant with these! Download a copy of the [Oniguruma regular expression syntax](http://www.geocities.jp/kosako3/oniguruma/doc/RE.txt) (used by Ruby) and keep it handy. Things to note:

-   Greedy, reluctant, and possessive matches

-   Lookbehind and lookahead --- your match rules are likely to make *extensive* use of these

-   Shy groups, named groups, named group references, and "variables" (the "Tanaka Akira Special", allowing you to refer *forward* to a group by number or name) --- I found named groups, in particular, extremely helpful for clarifying complicated expressions

-   `\G` --- This is a way of anchoring an attempted match in exactly the place where the previous match ended (useful when a grammar works by carving up a document *completely* into contiguous scopes)

It will also be a good idea to keep [Rubular](http://www.rubular.com) open in your browser. It allows you to test regular expressions, indicating matched groups.

Bear in mind, however, that because of the way the TextMate parser surveys your document, all regular expressions used in grammar match rules must apply to a *single line at a time*. A single expression cannot embrace multiple lines. Thus it is possible to write a regular expression that appears to work in Rubular (or TextMate's regex Find) but will fail as part of a grammar.

Another pitfall is that TextMate will not complain if you write a bad regular expression; the expression will just fail silently. That's probably a good thing in general, but it can drive you mad while trying to understand why your regular expression isn't working. This is another reason why Rubular is useful; it will show a descriptive error message if the expression itself is syntactically faulty.

Regular expressions will be surrounded by single quotes in the grammar. To express a single quote within a regular expression in a grammar, use two single quotes in succession.

Grammar Structure
-----------------

We now come to the actual apparent structure of a language grammar. I say "apparent" because I'm just guessing, based on my experience. The documentation is shockingly uninformative about this. There is no formal complete definitive specification, so far as I have discovered, of the structure of a grammar. Moreover, many aspects of a grammar are described in separate and scattered documents that can be difficult to discover. You should certainly keep on hand a copy of the [grammar page in the manual](http://manual.macromates.com/en/language_grammars#language_grammars), as well as James Edward Gray II's [book](http://pragprog.com/book/textmate/textmate). But I will attempt to do a better job here of assembling and summarizing the facts.

### Property List Syntax

The grammar is portrayed by the bundle editor as an old-style property list. Under the hood, however, it is a new-style property list, i.e. XML. This means that every time you save, the grammar is "compiled" into a new-style property list, with an error dialog if you've made a mistake such that this can't be done; and every time you abandon the grammar window, when you return to it, the window is repopulated by "decompiling" the saved XML into an old-style property list. Therefore:

-   The grammar contents may not look the same when you return to it as when you left it; for example, things whose order does not matter (dictionary elements) may appear in a different order.

-   If you leave the grammar window after seeing the error dialog and without correcting the error, *you will lose your recent work*, because your recent work wasn't saved, and now it has been wiped out by reading and decompiling the previously saved XML from disk.

    (This is more of a problem than you might expect, because the bundle editor window is responsive to clicks when it is not frontmost. Thus, you might switch to another window to study something about why your property list might be invalid, then click on the bundle editor window to switch back, and discover that, because of *where* you clicked, you've accidentally left your grammar window and have lost all your recent work. I know this from experience, obviously! My solution is: before switching away from the bundle editor with unsaved changes, copy its contents and paste them into BBEdit for safekeeping.)

An old style property list has two kinds of collection: arrays and dictionaries.

-   An array, bounded by parentheses `()`, is an *ordered* list of elements separated by *comma*. It does no harm to follow the last element with a comma as well, and I recommend that you do so.

-   A dictionary, bounded by curly braces `{}`, is an *unordered* list of name--value pairs separated by and ending with *semicolon*. A name--value pair is joined by an equal sign. The name does not have to be quoted, and usually will not be, unless it contains spaces or numbers. String values will be single quoted.

    **NOTE**: In the grammar structure, *any* dictionary can contain a `comment` entry. This can be extremely useful and I recommend liberal use of this feature.

### Top Level Structure

Before I can describe the top level of a language grammar, I need to tell you that a *match rule* (also called a *pattern*) is a dictionary. But I'm not going to tell you what's *in* a match rule until later.

The top level, in the TextMate 2 bundle editor window, is a dictionary --- that is, it's a pair of curly braces `{}`. Some of the entries in the real underlying dictionary are now edited through fields in the bundle editor drawer. Of these, the most important is the grammar's scope, listed in the drawer's Grammar field. This is the main scope that will be assigned to the entire document. It is typically a specialization of some existing scope, in order to acquire related bundle-based features, though not usually in such a way that an existing grammar will be injected automatically on top of yours.

For example, `text.html.markdown` is not magically imposed upon by the Text bundle's grammar (which is scoped to `text.plain`) or by the HTML bundle's grammar (which is scoped to `text.html.basic`). But because it is a specialization of `text.html`, it does magically acquire any features scoped to `text.html`, such as the Command-Ampersand keyboard shortcut from the HTML bundle.

On the other hand, in the Bundle Development bundle, the Language Grammar grammar is scoped as `source.plist.textmate.grammar` exactly so that the Property List bundle's Property List (Old-Style) grammar (`source.plist`) *can* be magically imposed upon it; the former is a pure specialization of the latter.

From here on, let's confine ourselves to what's shown in the bundle editor window itself. The top-level dictionary can contain (aside from a comment) a `patterns` array and (optionally) a `repository` dictionary, and that's all.

-   A `patterns` array is an array of match rules. The fact that it's an array is important, because *order matters*. Matches are performed *in the order listed*. TextMate considers lines of a document one at a time, looking within each one for matches. When we make a match, that's the end for everything in that line up to that point. Thus, you can effectively make conditional rules by judicious ordering of a `patterns` array.

-   A repository is a dictionary, and it may contain two kinds of thing:

    -   A repository may contain named *match rules*. The fact that the rules are *named* is important, because this means that a match rule can refer to another match rule *by name*, provided the latter is in a repository.

    -   A repository may contain named *top levels*. That is, a repository entry might be a name paired with a dictionary containing a `patterns` array and (optionally) a `repository` dictionary.

        The `patterns` array inside a named dictionary inside a repository provides a way of naming (and thus referring to) a whole bunch of match rules simultaneously.

        The `repository` inside a named dictionary inside a repository has no structural significance --- it is not special merely because it is at a deeper level --- but it is certainly an organizational convenience, especially because the bundle editor gives you structural code folding.

To illustrate and explain, here's the structure of the Markdown grammar:

```
{
  patterns = (... match rules ...);
  repository = {
    block = {
      patterns = (... match rules ...);
      repository = {
        blockquote = {... one match rule ...};
        ...
      };
    };
  };
}

```

With that structure, the name `block` can be used to refer to all of the `block` > `patterns` match rules simultaneously. The `blockquote` match rule, despite its depth (a repository within a repository), can be referred to from anywhere.

![image](https://www.apeth.com/nonblog/images/grammarTopLevel.png)

Match Rules
-----------

A match rule, also known as a *pattern*, is where the power lies in a language grammar. It is the part of the grammar that actually instructs TextMate's parser what to do: "as you walk through the text, look for this pattern and assign this scope."

A match rule, as I've already said, is a dictionary. In describing the overall structure of the language grammar, I've already told you the two main places where match rules can go:

-   A match rule can be one entry in a `patterns` array.

-   A match rule can be a named dictionary in a repository.

I have not, however, said anything yet about what is *inside* a match rule dictionary. Now it's time to do that. There are actually *three* possible structures for the contents of a match rule:

-   An `include`.

-   A `match` pattern.

-   A `begin`/`end` (or `begin`/`while`) pattern.

We will consider each in turn. But first, I'll mention two things that *any* match rule can contain:

-   A comment. This is a name--value pair named `comment`.

-   An off switch. This is a name--value pair named `disabled`, with value `1`. This is a convenient way to experiment or develop, leaving a rule in place while effectively commenting it out.

### An Include Match Rule

An `include` match rule is a way of saying: "Substitute for me the *named* match rule(s) I hereby specify." Such a match rule contains, I believe, just one name--value pair. The name of that pair is `include`. The value is usually the name of something in the repository at any level, *preceded by a hash-sign*. (But it cannot, I believe, be `#repository`, even though that might be the name of something in the repository.)

To see include rules in action, let's return to and fill out a little further the structure of the Markdown grammar:

```
{
  patterns = (
    { include = '#block'; }
  );
  repository = {
    block = {
      patterns = (
        { include = '#separator'; };
        { include = '#heading'; };
        { include = '#blockquote'; };
        ...
      );
      repository = {
        blockquote = {... one match rule ...};
        heading = {... one match rule ...};
        ...
        separator = {... one match rule ...};
      };
    };
  };
}

```

Consider how, by means of include rules, the `blockquote` match rule is actually brought into play. There is only one top-level match rule --- the `include` rule specifying `#block`. This rule effectively substitutes for itself the entire array of `patterns` listed under `block` in the repository. These patterns are themselves all include rules. The result is exactly as if the top level `patterns` array *were* the include rule for `#separator`, then the include rule for `#heading`, then the include rule for `#blockquote`, and so on.

Those include rules, in turn, are virtually replaced by the rules that their values name --- the *actual* `separator` rule from the repository, the *actual* `heading` rule from the respository, the *actual* `blockquote` rule from the repository, and so forth. Those actual rules are all defined in the second-level repository.

(Note that the order of the include rules in the `patterns` array is significant; an array is ordered. The order of the actual rules in the second-level repository, on the other hand, is undefined and unimportant; it happens to be shown as alphabetical, and that's convenient, but it's just a detail of the display implementation.)

The value of an `include` rule can alternatively be a *scope name*. This is a good way to inject an entire grammar inside yours. For example, in my AsciiDoc bundle, I assume that the contents of a passthrough block (never mind what that is) will be XML. Therefore, inside my match rule for a passthrough block, I have an `include` rule specifying `text.xml`. This causes the whole grammar from the XML bundle to come into play in this region of my document. In addition, TextMate 2 permits a single named item to be plucked from a foreign repository using the syntax `scopeName#itemName`.

The value of an `include` rule can also be `$self`, meaning the whole current grammar, or `$base`, the grammar inside which we are embedded. They are used by several prominent grammars (such as the Objective-C grammar), but I have never used them and I don't quite understand the details.

As I mentioned at the outset, as far as I can tell, if a match rule contains an `include` key, that is its *only* key (except possibly for `comment` and `disabled`).

![image](https://www.apeth.com/nonblog/images/grammarMatchRuleType1.png)

### A One-Pattern Rule

A one-pattern rule must contain at least this key--value pair:

-   The `match` key. Its value is a regular expression that the TextMate parser is to look for.

    This is, I am at pains to stress, a *single-line* regular expression. The TextMate parser will not examine more than one line of text at a time; thus, a regular expression that includes a newline (`\n`) anywhere other than as the last matched character will never match. By the same token, though, it can be quite useful (especially in a "prose" language grammar such as the Markdown grammar) to end a regular expression with `$\n?` as a way of snarfing up the newline at the end of the line if there is one.

A one-pattern rule may also contain this key--value pair:

-   The `name` key. Its value is a scope, or an expression that evaluates to a scope. This is the scope that will be applied to the matched text. This scope assignment, in turn, has far-reaching implications for how the matched text will be styled by themes and settings, as I've already described above.

For example, here's the rule from the Markdown grammar repository that picks out certain characters preceded by backslash:

```
escape = {
    name = 'constant.character.escape.markdown';
    match = '\\[-`*_#+.!(){}\[\]\\>]';
};

```

This means that any matches we find using the `escape` rule will be assigned the `constant.character.escape.markdown` scope. A theme or setting may then come along and style any `constant.character.escape` text.

The scope name can be, as I mentioned earlier, "an expression that evaluates to a scope". What do I mean by *that*? Well, the scope name is actually what TextMate calls a *format string*. For more about these, see [this article](http://blog.macromates.com/2011/format-strings/). The syntax here is derived from shell programming syntax. With a format string, you can do things such as:

-   Use a matched group as a term: Refer to it as `$0`, `$1`, and so forth.

-   Use the value of a global variable as a term.

-   Perform a transform, such as downcasing, on the value of a term.

-   Do a find/replace on the value of a term.

-   Supply a different result depending on whether a term (such as a matched group) is defined or not.

A one-pattern rule may also contain this key--value pair:

-   The `captures` key. Its value is a dictionary. In this dictionary, each key is a number or name corresponding to a matched group from the `match` expression, and the corresponding value is a dictionary. This dictionary may contain one or both of these entries:

    -   A `name`. This is the scope that will be assigned to the matched group text.

    -   A `patterns` array. This is *a list of match rules* to be sought *within the matched group text* --- thus permitting the search for matches to continue inside the matched text.

This use of the `captures` > `patterns` structure is extremely important. Without it, a stretch of text matched by a match rule is considered finished, and TextMate's search proceeds to the rest of the line *after* the matched text. With it, the stretch of matched text itself becomes a candidate for further matches.

Here's a simple minimal example. Imagine a toy markup language in which bold is delimited by asterisks and italic is delimited by underlines:

```
{ patterns =
  (
    {
      name = 'markup.bold.toy';
      match = '\*.*?\*';
    },
    {
      name = 'markup.italic.toy';
      match = '_.*?_';
    },
  );
}

```

This works, but only for bold and italic stretches of text that are *separate*, like this:

```
*This is bold.* _This is italic._

```

An italic stretch cannot occur inside a bold stretch, because once a stretch of text has been matched as bold, it is discarded from further consideration. If we want italic inside bold, and vice versa, to be possible, we can use the `captures` > `patterns` structure to duplicate the italic pattern inside the bold pattern and the bold pattern inside the italic pattern:

```
{ patterns =
  (
    {
      name = 'markup.bold.toy';
      match = '\*.*?\*';
      captures = {
        0 = {
          patterns = (
            {
              name = 'markup.italic.toy';
              match = '_.*?_';
            }
          );
        };
      };
    },
    {
      name = 'markup.italic.toy';
      match = '_.*?_';
      captures = {
        0 = {
          patterns = (
            {
              name = 'markup.bold.toy';
              match = '\*.*?\*';
            }
          );
        };
      };
    },
  );
}

```

That sort of thing is perfectly legal --- the `patterns` array is a list of match rules, and we are providing match rules. And it works, correctly marking up text such as this:

```
*This is bold _containing italic_.*
And _this is italic *containing bold*._

```

In this case, though, a neater approach would be to move the match rules into the repository so that they can be referred to by name, and use the `captures` > `patterns` structure *with the `patterns` match rules being `include` rules*, to continue the search for italic inside a matched bold stretch of text, and vice versa:

```
{ patterns = (
    { include = '#bold'; },
    { include = '#italic'; },
  );
  repository = {
    bold = {
      name = 'markup.bold.toy';
      match = '\*.*?\*';
      captures = { 0 = { patterns = ( { include = '#italic'; } ); }; };
    };
    italic = {
      name = 'markup.italic.toy';
      match = '_.*?_';
      captures = { 0 = { patterns = ( { include = '#bold'; } ); }; };
    };
  };
}

```

**WARNING:** Do *not* put a `patterns` array at the top level of a one-pattern match rule. It doesn't generate any explicit error, but it doesn't work correctly either. So, for example, *this is wrong*:

```
name = 'markup.bold.toy';
match = '\*.*?\*';
patterns = ( { include = '#italic'; } );

```

You are getting the benefit of my hard-fought experience here; it took me about a week of wrestling (and some direct help from Allan Odgaard) to learn the right way to do an `include` inside a one-pattern match rule.

![image](https://www.apeth.com/nonblog/images/grammarMatchRuleType2.png)

### A Two-Pattern Rule

A two-pattern rule must contain *two* regular expressions: either `begin` and `end`, or (new in TextMate 2) `begin` and `while`. These are still single-line regular expressions (the TextMate parser does not consider more than one line at a time), but there are two of them, so together they may delimit a stretch of text that embraces multiple lines.

-   The `begin` key is a regular expression that will be sought initially.

-   If the `begin` regular expression generates a match, TextMate will start looking immediately after it (starting in the same line, if the `begin` match did not snarf up the entire line) for the `end` or `while` expression.

    -   If you provided an `end` expression, TextMate will keep walking the document until it matches the `end` expression, and will stop.

    -   If you provided a `while` expression, TextMate will keep walking the document until it comes to a line where the `while` expression fails.

Neither of these pairs works quite the way I would have intuitively expected, so I'm going to say some more about them.

With `begin`/`end`, if the `end` pattern is *not* found, the overall match *does not fail*: rather, once the `begin` pattern is matched, the overall match runs to the `end` pattern *or to the end of the document*, whichever comes first. The underlying architectural reason is that the TextMate parser does not backtrack; once the `begin` pattern is matched, it is matched successfully and that's that --- TextMate can't change its mind and decide that it shouldn't have matched the `begin` pattern after all.

This is a major limitation in the degree of intelligence a language grammar can exhibit; it is said to be due to the need for speed, and perhaps for simplicity. In any case, it is intentional, and some grammars actually take deliberate advantage of this behavior. For example, the Property List Old-Style grammar starts like this:

```
{ patterns = (
    { begin = '(?=\(|\{)';
      end = '(?=not)possible';

```

The expression `(?=not)possible` is, uh, not possible; the next character can't be "n" if the next character is "p". This `end` pattern is designed to fail. The whole pair thus means: Once you have encountered a left parenthesis or a left curly brace, immediately snarf it up along with the entire remainder of the document (and just about everything else in the grammar then works through `include` rules inside that snarfed-up material).

With `begin`/`while`, things are even stranger. It's rather difficult for me to deduce what the rules are; they are not yet documented, and I know of only one grammar that uses `begin`/`while`, namely the Markdown grammar, so my available examples are quite limited. Here's what I've been able to guess:

1.  Unlike `begin`/`end`, the text matched by `begin`/`while` stops at the end of the current line. Thus, if the `begin` is encountered and the `while` is never encountered, only *the rest of the line* containing the `begin` text is subsumed into the matched text.

2.  After encountering a `begin` match, the TextMate parser looks in the *next line* to see if it can match the `while` pattern. If it can, it incorporates the matched text *and the entire rest of that line*, and looks in the *next* line to see if it can match the `while` pattern. This continues until a line is encountered in which the `while` pattern cannot be matched.

Again, there is no backtracking; failure to find the `while` pattern at all is not a reason for rejecting the `begin` match that was already found. Notice also that nothing about these rules says that the resulting matched stretches of text have to be contiguous. That's because they *don't* have to be contiguous! So, for example, let's say that the `begin` pattern matches an asterisk (`*`) and the `while` pattern matches a plus sign (`+`). Then (matching stretches are shown in caps):

```
This is +no match.
This is *A MATCH.

This is +no match.
This is *A MATCH.
This is +ALSO A MATCH.
This is +A MATCH TOO.

This is +no match.

```

Well, I *told* you it was strange! However, it seems likely that I'm abusing the `begin`/`while` rule with this example. This curious "from here to the end of the line" match behavior is probably intended for patterns that mark an *entire* line in terms of *how it begins*. You'll notice that *all* the Markdown grammar uses of the `begin`/`while` work that way. For example, the `blockquote` rule is written like this:

```
begin = '(^|\G)(>) ?';
while = '(^|\G)(>) ?';

```

That means, in essence: match an entire line that begins with one (`begin`) or more (`while`) greater-than signs (and perhaps one space after the greater-than sign), and keep matching entire *successive* lines (`while`) that begin that way.

Now let's talk about other key--value pairs that can appear in a two-pattern rule.

-   The `name` key. This is the scope (or an expression evaluating to a scope) to be applied to the *entire* matched stretch(es) of text starting at the start of the `begin` match.

-   The `contentName` key. This is the scope to be applied to what's *between* the `begin` match and the `end` match (or the end of the document). (With `begin`/`while`, this isn't illegal, but it's pointless, as it covers exactly the same stretch(es) as the `name` scope.)

-   The `beginCaptures` and `endCaptures` (or `whileCaptures`) keys. These work like the `captures` key in the one-pattern rule: the value is a dictionary each of whose entries refers to a match group in the `begin`, `end`, or `while` pattern respectively, and is itself a dictionary containing `name` or `patterns` (or both). Thus you can apply a scope to, and/or continue searching inside, a matched stretch of text. If the name or number of the matched group happens to be the same for both the `begin` pattern and the other pattern, you can (if appropriate) use `captures` as a shorthand to avoid saying the same thing twice.

-   The `patterns` key. This is like the `patterns` key inside the `captures` and `beginCaptures` (and so forth) dictionaries, but it applies to the region *between* the `begin` and `end` matches. Again, this is so that you can continue searching inside this stretch of text, which otherwise would be marked down as completed (and TextMate would start matching after the `end` match).

A surprise arises if a match specified in the `patterns` array can be satisfied by continuing beyond the `end` match. What happens is that the match *does* continue beyond the `end`, and causes the `end` to shift with it. For example, suppose we have this rule:

```
bold = {
  name = 'markup.bold.toy';
  begin = '\*';
  end = '\+';
  patterns = (
    { name = 'markup.italic.toy';
      match = '_.*?_';},
  );
};

```

If we start out like this, then things behave as we expect:

```
This is *a
bold+ stretch of text.

```

The words "a bold" are in bold, and that's the end of that. But now:

```
This is *a
_bold+ stretch_ of text.

```

I would have expected that since the italic rule can't be satisfied inside the matched bold stretch, it wouldn't be matched. However, TextMate has loaded the *entire second line* for examination, and thus succeeds in matching the words "bold stretch" as italic. Moreover, we have now "punched through" the original `end` of the bold stretch, and the *entire rest of the line* becomes bold, along with the *entire rest of the document* until and unless we come to *another* bold `end` match (a plus sign). I find this both surprising and disappointing, as it greatly limits the kinds of natural logical structure you can successfully express.

(In addition, we are told that, in case both a subpattern (from the `patterns` array) and the main `end` pattern would end exactly on the same character, the `end` pattern wins unless you set the `applyEndPatternLast` key to `1`. I don't understand what this means and I won't try to explain it.)

![image](https://www.apeth.com/nonblog/images/grammarMatchRuleType3.png)

Developing Your Grammar
-----------------------

In conclusion, some pearls of advice about the process of actual development of your grammar.

Collect all the documentation I've mentioned (including, I dare to suggest, this article) and leave everything open on your computer screen so that you can consult it as you work.

Now create your initial language grammar and start writing rules. Your first and constant question will be, at all times: "Is this working as I intend?" Thus you will need to have open, at all times, a TextMate document whose type is set to correspond to the grammar you are developing. As you create rules, add test text to the document so that you can see how it is affected.

The big issue, of course, is whether a given stretch of text is being assigned the scope you think it should be. Unfortunately, TextMate gives you no simple way to discover this. You cannot, for example, say "Show me all the scope runs of this document." Nor can you search for a scope within a document. So you need some other way to examine the scope(s) in force at a point of the document.

One important technique is to select some text and then press Control-Shift-P or Control-Shift-Command-P. These are commands from the Bundle Development bundle that display the current scopes(s) in a tooltip (which, unfortunately, vanishes if you so much as twitch the mouse).

Another clever device is to impose your own theme containing a special artificial scope (such as `text.testing`) that it styles in a very prominent way; you can then temporarily use that special scope as the `name` or `contentName` value in a rule to see just where that rule is applied in your test document. A newly created theme is empty, so you'll probably want to paste into your theme the contents of an existing theme. For example, copy the whole contents of the Mac Classic theme; make a new theme in your bundle; select the new theme's contents and paste the Mac Classic theme contents in its place; and now add a scope such as this:

```
{   scope = 'text.testing';
    settings = {
        foreground = '#FFFFFF';
        background = '#0000FF';
    };
},

```

Now bring your test document to the front and choose View > Theme > YourTheme to apply your special theme to it.

The bad news here is that if you make a change to your theme, it is not registered immediately in your test document; you have to choose View > Theme > YourTheme *again* in order to make any changes take effect. (Like themes, certain other changes are applied only lazily. For example, if your bundle is to have Folding settings or Table of Contents settings, you'll find that it can be quite difficult to nudge TextMate to reflect any changes you make in those settings.)

On the other hand, the good news --- the *really* good news --- is that any change you make in your *grammar* takes effect *as soon as you save*. Thus it is very easy to work simultaneously on the test document and the grammar. You can make a change in your grammar, save, and then immediately examine your test document to see how it is styled. If you're using the special theme trick that I just suggested, you can create or modify a rule and set its scope to `text.testing` to see that it carves up the document the way you expect, before changing its scope to the real value you ultimately want it to have.

Finally, *be prepared to crash*. This happens, presumably, because the combination of the grammar and the test document has driven TextMate temporarily insane. However, that's not bad; it's good, because it's better that you should crash TextMate *now*, while developing your grammar, than that TextMate should crash *later* and cause someone (maybe you) editing a document under the influence of your grammar to lose work on that document. Moreover, you probably will not lose any work on your grammar, because the evil change that you made in the grammar didn't go into effect until you saved; so, *ipso facto*, you *did* save. (You might lose work on your test document, but that's far less important. Still, you should be saving that test document often as well.)

To recover from such a crash, do *not* be a ninny (like me) and open the test document again in TextMate. That might merely cause TextMate to crash again! It is the combination of your grammar and *this test document* that is the source of the trouble. Instead, open TextMate without any document, get back to the bundle editor, and try to undo whatever bad thing you did that is causing the crash. It will not always be clear what this is, but I suspect that it is usually some sort of infinite recursion caused by a match rule containing an `include` rule containing itself.