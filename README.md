# SuperNuts2
A simple PRNG written in scheme. This is an experimental project made for educational purposes and just for fun too.

It generates pseudo-random numbers in the range 0.0 to 0.9999999[...]

It seems to pass the 'dieharder' test suite, and it has passed up to the 2 terabyte mark in testing with 'practrand'. The 'practrand' test is still running, I will keep this updated with the results as it continues. Testing is performed by multiplying by 4294967296.0 the 0.0 to 0.9999[...] output value of supernuts2 in order to create 32bit integers, and piping those to stdout and into 'practrand' and 'dieharder'.

Some testing was also performed by generating visual representations of the output stream - by plotting coloured pixels at pseudo-random positions in a 2D image, and by plotting transparent pixels at pseudo-random positions in a cube (spectral test)

Inspired by this quote from 'teach yourself scheme in fixnum days' by Dorai Sitaram: https://ds26gte.github.io/tyscheme/index-Z-H-7.html#TAG:__tex2page_call_footnote_Temp_5

"Writing your own version of random in Scheme requires quite a bit of mathematical chops to get something acceptable. We won’t get into that here."

So I took on the challenge.
