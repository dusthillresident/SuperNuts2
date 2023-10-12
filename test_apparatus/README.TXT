Assuming you have GCC and guile installed on your system, running "generate_u32.sh" should use supernuts2 to write a stream of pseudo-random bits to stdout.
You can then pipe that into other utilties, such as 'dieharder', 'practrand', etc.
