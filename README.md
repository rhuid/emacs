## Knot Emacs

- Builds on vanilla Emacs, which I believe is the best way to use Emacs. No bloat, no unnecessary layers or emulations.
- Assumes GNU Emacs 30.1 or higher.
- Uses use-package and use-package-vc. I have tried straight.el in the past and I believe use-package-vc (built-in) is cleaner, more explicit, leaner and makes Emacs boot up faster.
- Native compilation is enabled by default. Native compilation cuts down boot up time and Emacs starts faster overall.
- Implements user service to invoke emacs daemon at login. Using server-client model, Emacs starts up almost instantly.

### On the editor

To make editing more efficient and faster, I have redefined or reimplemented some of the built-in functions and added new ones. I will be adding new ones. Most are built from built-in functions and some extra juice (of course). One emphasis is to overload commands and make them context-sensitive, so that they behave as expected.

Over the years, I have realized that it's best to stay as close to vanilla emacs. One of the best things (if not the best) about Emacs is its hackability or extensibility. It's much more convenient to hack or extend vanilla emacs. However, I do like modal editing and I used meow as a minimal base to create a personalized editing system which, I believe, is superior to Evil mode (Vi/Vim emulation) in terms of efficiency, speed and leanness.

### Other things

- To enable Emacs keybindings for all GTK text fields (including Firefox), run in shell:

```bash
gsettings set org.gnome.desktop.interface
gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
```
