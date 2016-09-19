This is my own attempt to take Michael Feathers' code and modify it so as to fix the bug you pointed out and meet the new requirements you proposed. (It is also the first Haskell code I've written in my entire life, so don't be too surprised if it is not quite up to the standards of Haskell veterans...)

I started from the code in reflow-A.hs, which I changed in a couple of ways compared to the original. First I added an extra parameter to *lineBreak*, the maximum length of the line, because I can't imagine hardcoding that in a real application.

The second change was in the way whitespace is accounted for in the *brokenLineWordCount* pipeline. In the original code, that was (strangely enough) done in the first stage, by adding 1 to the length of each word. In the new one, the first stage returns the unchanged length of the words, and the space between them is accounted for in the second stage. Once that little quirk is cleaned up, not only the bug disappears, but the code follows a very simple and conceptually clear algorithm: we start from a sequence of words w₁ w₂ w₃…, we calculate their lengths l₁ l₂ l₃…, then we derive the lengths of the text obtained by concatenating the first n words L₁ L₂ L₃… using the formula L₁ = l₁, Lₙ = L(n-1) + lₙ + 1, and finally we count the number of words we can fit in the given space. Seems pretty straightforward to me, I can see no "cleverness" in it at all. There's also no undesirable coupling between the pipeline stages as far as I can tell. Although myself I would have probably merged the last two stages of the pipeline by writing a *countWhile* utility function, that would have mapped more directly to the above algorithm.

You also mentioned reading somewhere on Twitter the phrase: “I can’t think of any reasons for fragile code without some hard-to-define state”. Those words ring very true to me. It may not be the root of all evil, but ill-defined state/data is, in my own experience, a major source of bloody trouble. And this is a tiny but clear example: pass down the pipeline some hard-to-define data (the length of the word plus one? What's that? Why the + 1?), and bad things happen.

The "fictitious" change you proposed is now very easy to accommodate: just change "L(n-1) + lₙ + 1" to "L(n-1) + 2 * lₙ" in *brokenLineWordCount*, and of course reimplement *wordJoinedLines* from scratch. The code is in reflow-B.hs. There I also fixed a bug that I deliberately left in reflow-A.hs, for reasons that will become clear later.

Just one more observation: if we had wanted to, say, make the whitespace between the words the same length as the word that comes before it, instead of the one that follows it, we wouldn't have been able to do it by simply changing the closure passed to scanl1, because that closure can only access the next value in the sequence, not the previous one. But that doesn't mean the overall approach is doomed, because we could have written a custom version of scanl1 (assuming it isn't already in some library) that instead of passing only the next item in the list passes the entire input sequence and the index of the new value, or otherwise the part of the input sequence that has already been scanned (up to and including the next item, of course), or that same list reversed, or maybe a list zipper, or something else entirely, you name it. The change would still have been limited to the second stage of the pipeline inside *brokenLineWordCount*. And the new utility function so created would be fully reusable and actually plenty useful in other applications as well.

Let's now implement the language-agnostic hyphenation. My first attempt is in reflow-Cc.hs. I implemented a new pipeline stage, that takes the non-hyphenated line and the remaining text and adjusts them by applying the hyphenation. I inserted that new stage inside *brokenLines*, right after the *splitAt* call. That's the only change to the existing code, everything else is new.

In that first attempt I cheated a bit though, in order to minimize the amount of change in the existing code. First of all, I implemented a *fittingStringsCount* function which is just a more generic version of *brokenLineWordCount*. So now I'll have to reimplement the latter on top of the former. And I also packed the first two arguments to *hyphenatedLine* in a single tuple (in order to use the pipeline notation), but that's not what I would do in real code. My final code is in reflow-C.hs. The changes are syntactically more extensive, mainly as a result of the *fittingStringsCount* refactoring (which I don't think is a wholly negative thing: if anything, the ability to use the same function for both words and syllables is a plus, not a minus. *Better to have 100 functions...*), but the structure of the existing code has barely changed.

If you compare with a diff tool reflow-C.hs to reflow-Ar.hs, which is just the original code with the *fittingStringsCount* refactoring applied to it (not an entirely unreasonable thing to do, even in the absence of the hyphenation requirement), you'll see that the change to the existing code is basically limited to the addition of a single line/pipeline stage inside *brokenLines*.

Of course if I had applied the bug fix in reflow-B.ar to the original code as well, the change would have been a bit larger, as that logic has now moved to the hyphenation stage of the pipeline.

The new code is too complex to be written using a pipeline structure and the point-free notation (as far as I can see, at least), and it doesn't have the elegance of Michael Feathers' code, but it is still written in a "dataflow" style, which I find very readable (and which, by the way, is only made possible by lazy evaluation: without it that code would fail at runtime in several places). And that seems totally fine to me: when the problem is simple enough to be written (naturally, without jumping through hoops) in a pipeline style you just do that, if not you use something more complex.

I would also like to point out that adjusting the existing code to fit the new requirement took, ***in practice***, basically no time and no effort. The time-consuming part was to implement, test and debug (yes, I had bugs. Plural...) the new *hyphenatedLine* function, and then refactor it until I thought it was stylistically acceptable. So while I realize that with OO you might have been able to extend the existing code base without altering it (although I suspect that if that change had come unexpected, it might have required either some refactoring in the base classes or some code duplication in the derived ones. That's what would have happened to me, anyway) and keep both the hyphenated and non-hyphenated versions around (something that here would require the addition of an extra parameter to both *lineBreak* and *brokenLines*, I guess), and while I appreciate that that might be important in some situations (if you were for example developing a library for third-parties to use, or if your application had to support a lot of different configurations), in all the other cases (which would cover, in practice, 99.9% of my own working experience as a rank-and-file programmer) that would have made no difference at all.

Let's now tackle the last change, the language-aware hyphenation. I'm not sure how you would have gone about determining the language(s) being used, what I did was to define a new domain-specific type, *Word*, which combines a string and a language value, and a (fake, in this case) *detectLang* function that takes a list of *Strings* and returns a list of *Words*, with a guess for each word's language. The new code is in reflow-D.hs. All in all, what I had to do was to insert the new function/stage inside the *lineBreak* pipeline, change the type of one parameter from *String* to *Word* in the signature of both *brokenLines* and *brokenLineWordCount*, and strip off the language information in a couple of places, where it was either not expected or not needed anymore. That's in the old part of the code, the one written by Michael Feathers. In the new part I had to again change the type of a couple of parameters in *hyphenatedLine*, and reattach the language information to the trailing part of the word that I was trying to split. And of course *syllables* has to be completely rewritten, but that's expected in this case, I suppose. Again, please do a diff between reflow-D.hs and reflow-C.hs (or reflow-Ar.hs). All the changes are really straightforward. Besides, you yourself said that the changes needed to justify the text are acceptable, as they naturally fit in the "seams" of the code. Is the implementation of either language-agnostic and language-aware hyphenation really that different?

So, after all is said and done, it seems to me that this approach you criticize so vehemently is a hell of lot more resilient than you are willing to give it credit for. Even the introduction of a new domain-specific type didn't significantly change its structure. It mainly changed the signatures of some of the functions. (And at this point, somebody else might point out that that could have been avoided too, by relying more on type inference. Not me though, I really like my explicit function signatures...).
I also disagree with your suggestion about using a Document object to keep around both the original and the reformatted versions of the text, but never mind that now, that's maybe another story for another day.

I confess that your paper has left me quite baffled. It felt like we weren't even looking at the same code at all. What did I miss?
