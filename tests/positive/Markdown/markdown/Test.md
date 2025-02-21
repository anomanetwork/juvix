# Example

What is important is seldom urgent.

A Juvix Markdown file name ends with `.juvix.md`. This kind of file must contain
a module declaration at the top, as shown below ---in the first code block.

<pre class="highlight"><code class="juvix"><pre class="src-content"><span class="ju-keyword">module</span> <span id="YTest"><span class="annot"><a href="X#YTest" class="ju-code-link ju-module"><span class="annot"><a href="X#YTest" class="ju-code-link ju-module"><span class="ju-module"><span class="ju-module">Test</span></span></a></span></a></span></span><span class="ju-delimiter">;</span><br/></pre></code></pre>

Certain blocks can be hidden from the output by adding the `hide` attribute, as shown below.



<pre class="highlight"><code class="juvix"><pre class="src-content"><span class="ju-keyword">axiom</span> <span id="YX"><span class="annot"><a href="X#YX" class="ju-code-link ju-axiom"><span class="annot"><a href="X#YX" class="ju-code-link ju-axiom"><span class="ju-axiom">X</span></a></span></a></span></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#YL" class="ju-code-link ju-axiom"><span class="ju-axiom">L</span></a></span><span class="ju-delimiter">;</span></pre></code></pre>

The `extract-module-statements` attribute can be used to display only the statements contained in a module in the output.

<pre class="highlight"><code class="juvix"><pre class="src-content"><span class="ju-keyword">type</span> <span id="YFoo1.T"><span class="annot"><a href="X#YFoo1.T" class="ju-code-link ju-inductive"><span class="annot"><a href="X#YFoo1.T" class="ju-code-link ju-inductive"><span class="ju-inductive">T</span></a></span></a></span></span> <span class="ju-keyword">:=</span> <span id="YFoo1.T.t"><span class="annot"><a href="X#YFoo1.T.t" class="ju-code-link ju-constructor"><span class="annot"><a href="X#YFoo1.T.t" class="ju-code-link ju-constructor"><span class="ju-constructor">t</span></a></span></a></span></span><span class="ju-delimiter">;</span></pre></code></pre>

You can pass a number to the `extract-module-statements` attribute to drop that number of statements from the start of the module.

<pre class="highlight"><code class="juvix"><pre class="src-content"><span class="ju-keyword">open</span> <span class="annot"><a href="X#YFoo2.T" class="ju-code-link ju-module"><span class="ju-module">T</span></a></span> <span class="ju-keyword">using</span> <span class="ju-delimiter">{</span><span class="annot"><a href="X#YFoo2.T.t" class="ju-code-link ju-constructor"><span class="ju-constructor">t</span></a></span><span class="ju-delimiter">}</span> <span class="ju-keyword">public</span><span class="ju-delimiter">;</span><br/><br/><span id="YFoo2.a"><span class="annot"><a href="X#YFoo2.a" class="ju-code-link ju-function"><span class="annot"><a href="X#YFoo2.a" class="ju-code-link ju-function"><span class="ju-function">a</span></a></span></a></span></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#YFoo2.T" class="ju-code-link ju-inductive"><span class="ju-inductive">T</span></a></span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#YFoo2.T.t" class="ju-code-link ju-constructor"><span class="ju-constructor">t</span></a></span><span class="ju-delimiter">;</span></pre></code></pre>

Commands like `typecheck` and `compile` can be used with Juvix Markdown files.

<pre class="highlight"><code class="juvix"><pre class="src-content"><span id="Ymain"><span class="annot"><a href="X#Ymain" class="ju-code-link ju-function"><span class="annot"><a href="X#Ymain" class="ju-code-link ju-function"><span class="ju-function">main</span></a></span></a></span></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#YNat" class="ju-code-link ju-inductive"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#YNat.zero" class="ju-code-link ju-constructor"><span class="ju-constructor">zero</span></a></span><span class="ju-delimiter">;</span></pre></code></pre>

Other code blocks are not touched, e.g:

```text
This is a text block
```


```haskell
module Test where
```

Blocks indented.

  ```haskell
    module Test where
  ```

Empty blocks:

```
```

We also use other markup for documentation such as:

!!! note

    We use this kind of markup for notes, solutions, and other stuff

    1. More text

        ```text
        f {n : Nat := 0} {m : Nat := n + 1} ....
        ```

    2. Second text


??? info "Solution"

    Initial function arguments that match variables or wildcards in all clauses can
    be moved to the left of the colon in the function definition. For example,

    <pre class="highlight"><code class="juvix"><pre class="src-content"><span class="ju-keyword">module</span> <span id="Ymove-to-left"><span class="annot"><a href="X#Ymove-to-left" class="ju-code-link ju-module"><span class="annot"><a href="X#Ymove-to-left" class="ju-code-link ju-module"><span class="ju-module">move-to-left</span></a></span></a></span></span><span class="ju-delimiter">;</span><br/>  <span class="ju-keyword">import</span> <span id="YLib"><span class="annot"><a href="X#YLib" class="ju-code-link ju-module"><span class="annot"><a href="X#YLib" class="ju-code-link ju-module"><span class="ju-module"><span class="ju-module">Lib</span></span></a></span></a></span></span> <span class="ju-keyword">open</span><span class="ju-delimiter">;</span><br/>  <span id="Ymove-to-left.add"><span class="annot"><a href="X#Ymove-to-left.add" class="ju-code-link ju-function"><span class="annot"><a href="X#Ymove-to-left.add" class="ju-code-link ju-function"><span class="ju-function"><br/>  add</span></a></span></a></span></span> <span class="ju-delimiter">(</span><span class="annot"><a href="X#YTest:16" class="ju-code-link ju-var"><span class="ju-var">n</span></a></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#YNat" class="ju-code-link ju-inductive"><span class="ju-inductive">Nat</span></a></span><span class="ju-delimiter">)</span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#YNat" class="ju-code-link ju-inductive"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">-&gt;</span> <span class="annot"><a href="X#YNat" class="ju-code-link ju-inductive"><span class="ju-inductive">Nat</span></a></span><br/>    <span class="ju-keyword">|</span> <span class="annot"><a href="X#YNat.zero" class="ju-code-link ju-constructor"><span class="ju-constructor">zero</span></a></span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#YTest:16" class="ju-code-link ju-var"><span class="ju-var">n</span></a></span><br/>    <span class="ju-keyword">|</span> <span class="annot"><a href="X#YNat.suc" class="ju-code-link ju-constructor"><span class="ju-constructor"><span class="ju-delimiter">(</span>suc</span></a></span> <span id="YTest:17"><span class="annot"><a href="X#YTest:17" class="ju-code-link ju-var"><span class="annot"><a href="X#YTest:17" class="ju-code-link ju-var"><span class="ju-var">m</span></a></span></a></span></span><span class="ju-delimiter">)</span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#YNat.suc" class="ju-code-link ju-constructor"><span class="ju-constructor">suc</span></a></span> <span class="annot"><a href="X#Ymove-to-left.add" class="ju-code-link ju-function"><span class="ju-function"><span class="ju-delimiter">(</span>add</span></a></span> <span class="annot"><a href="X#YTest:16" class="ju-code-link ju-var"><span class="ju-var">n</span></a></span> <span class="annot"><a href="X#YTest:17" class="ju-code-link ju-var"><span class="ju-var">m</span></a></span><span class="ju-delimiter">)</span><span class="ju-delimiter">;</span><br/><span class="ju-keyword">end</span><span class="ju-delimiter">;</span></pre></code></pre>

    is equivalent to

    <pre class="highlight"><code class="juvix"><pre class="src-content"><span class="ju-keyword">module</span> <span id="Yexample-add"><span class="annot"><a href="X#Yexample-add" class="ju-code-link ju-module"><span class="annot"><a href="X#Yexample-add" class="ju-code-link ju-module"><span class="ju-module">example-add</span></a></span></a></span></span><span class="ju-delimiter">;</span><br/>  <span class="ju-keyword">import</span> <span id="YLib"><span class="annot"><a href="X#YLib" class="ju-code-link ju-module"><span class="annot"><a href="X#YLib" class="ju-code-link ju-module"><span class="ju-module"><span class="ju-module">Lib</span></span></a></span></a></span></span> <span class="ju-keyword">open</span><span class="ju-delimiter">;</span><br/>  <span id="Yexample-add.add"><span class="annot"><a href="X#Yexample-add.add" class="ju-code-link ju-function"><span class="annot"><a href="X#Yexample-add.add" class="ju-code-link ju-function"><span class="ju-function"><br/>  add</span></a></span></a></span></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#YNat" class="ju-code-link ju-inductive"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">-&gt;</span> <span class="annot"><a href="X#YNat" class="ju-code-link ju-inductive"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">-&gt;</span> <span class="annot"><a href="X#YNat" class="ju-code-link ju-inductive"><span class="ju-inductive">Nat</span></a></span><br/>    <span class="ju-keyword">|</span> <span id="YTest:18"><span class="annot"><a href="X#YTest:18" class="ju-code-link ju-var"><span class="annot"><a href="X#YTest:18" class="ju-code-link ju-var"><span class="ju-var">n</span></a></span></a></span></span> <span class="annot"><a href="X#YNat.zero" class="ju-code-link ju-constructor"><span class="ju-constructor">zero</span></a></span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#YTest:18" class="ju-code-link ju-var"><span class="ju-var">n</span></a></span><br/>    <span class="ju-keyword">|</span> <span id="YTest:19"><span class="annot"><a href="X#YTest:19" class="ju-code-link ju-var"><span class="annot"><a href="X#YTest:19" class="ju-code-link ju-var"><span class="ju-var">n</span></a></span></a></span></span> <span class="annot"><a href="X#YNat.suc" class="ju-code-link ju-constructor"><span class="ju-constructor"><span class="ju-delimiter">(</span>suc</span></a></span> <span id="YTest:20"><span class="annot"><a href="X#YTest:20" class="ju-code-link ju-var"><span class="annot"><a href="X#YTest:20" class="ju-code-link ju-var"><span class="ju-var">m</span></a></span></a></span></span><span class="ju-delimiter">)</span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#YNat.suc" class="ju-code-link ju-constructor"><span class="ju-constructor">suc</span></a></span> <span class="annot"><a href="X#Yexample-add.add" class="ju-code-link ju-function"><span class="ju-function"><span class="ju-delimiter">(</span>add</span></a></span> <span class="annot"><a href="X#YTest:19" class="ju-code-link ju-var"><span class="ju-var">n</span></a></span> <span class="annot"><a href="X#YTest:20" class="ju-code-link ju-var"><span class="ju-var">m</span></a></span><span class="ju-delimiter">)</span><span class="ju-delimiter">;</span><br/><span class="ju-keyword">end</span><span class="ju-delimiter">;</span></pre></code></pre>
