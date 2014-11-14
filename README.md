# spritz

This is an implementation of the
[Spritz](https://people.csail.mit.edu/rivest/pubs/RS14.pdf) spongy RC4-like
stream cipher which was documented by *Ronald L. Rivest* and *Jacob C. N.
Schuldt*.

This implementation is a direct one, based on the pseudocode presented in the
original paper. As such, we make very heavy use of the State monad (and the
`lens` library to ease our use of it). Future work could (and should) be done to
limit or eliminate our use of State.

The functions provided align with the functions presented in the paper, aside
from slight naming and style convention changes.

## License

BSD-2. See `LICENSE` for more information.
