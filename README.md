## Knot Emacs

- Builds on vanilla Emacs, which I believe is the best way to use Emacs. No bloat, no unnecessary layers or emulations.
- Assumes GNU Emacs 30.1 or higher.
- Uses use-package and use-package-vc. I have tried straight.el in the past and I believe use-package-vc (built-in) is cleaner, more explicit, leaner and makes Emacs boot up faster.
- Implements Makefile to build native .eln files. Native compilation cuts down boot up time and makes Emacs faster overall.
- Implements user service to invoke emacs daemon at login. Using server-client model, Emacs starts up almost instantly (on par with terminal apps like Yazi or NeoVim)

### On the editor

Over the years, I have realized that it's best to stay as close to vanilla emacs. However, I do like modal editing and I used meow as a base to create a personalized editing system which, I believe, is superior to Evil mode (Vi/Vim emulation) in terms of efficiency, speed, leanness and closeness to vanilla.
