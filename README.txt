Layers is a generative music library for common lisp, using slippery chicken and CLM for a lot of its utilities and Pure Data as a real time sampler. It's purpose is not the sound design or synthesis but rather that of a complex analysis and sampling system. Layers might be used in real time or to help realize fixed media pieces, for example using #'export-with-clm or #'fplay, or by just creating data structures.

Slippery chicken should already be up and running in order to use this library. 
Then load the src/all.lsp file and check scores/example.lsp to see an example of how to use Layers.

For real time use, Layers connects with osc (using the #'start-osc function) to Pure Data, to trigger samples. Ideally, both Lisp and Pure Data should be able to access the samples. It could however also be set up in a way, that only Pure Data needs to access the samples for it to trigger them. Lisp and Pure Data could run on different machines. 

If anyone ever uses this and there is any questions contact me at contact@leonfocker.de
