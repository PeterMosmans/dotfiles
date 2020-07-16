# Changelog

All notable changes to this project will be documented in this file. See [standard-version](https://github.com/conventional-changelog/standard-version) for commit guidelines.

## 1.1.0 (2020-07-16)


### Features

* add openwith package to open files using system applications ([c7e342a](https://github.com/PeterMosmans/dotfiles/commit/c7e342ac016622d4f1e0c9ebb08e83847692f61b))
* add zsh emacs plugin ([0908a93](https://github.com/PeterMosmans/dotfiles/commit/0908a93f3c331c348d2fb1db05021cdba5c5ef23))
* open more file types with default / system handlers ([7106ff2](https://github.com/PeterMosmans/dotfiles/commit/7106ff204b779396674d102ab254d987e515c7a4))
* set bat and tmux themes early on ([0d1bf0b](https://github.com/PeterMosmans/dotfiles/commit/0d1bf0b5fcf2424e87f1134704f7ad58a4bb0f52))
* use emacs-nox by default under GNU/Linux ([586081b](https://github.com/PeterMosmans/dotfiles/commit/586081b06ed06ccff0d06eb6ea5c5c472d80865a))
* **ag:** add basic ignore file for ag AKA the silver searcher ([c870d96](https://github.com/PeterMosmans/dotfiles/commit/c870d963c01cceee30d46c7958b255043e632bd3))
* **agignore:** don't search svg files ([e4f73f4](https://github.com/PeterMosmans/dotfiles/commit/e4f73f4a939bf7c81491f08bbddb6a3ce278e91f))
* **init.el:** add company backends jedi and shell ([949214b](https://github.com/PeterMosmans/dotfiles/commit/949214b59ebf0266e96e24ce73a2d3aabe245e4d))
* **init.el:** add flymake packae for shellcheck ([fa287e7](https://github.com/PeterMosmans/dotfiles/commit/fa287e77e9581cc5aaec41779d938902e6e89c55))
* **init.el:** add fonts for Unicode blocks ([d040b65](https://github.com/PeterMosmans/dotfiles/commit/d040b656853dddd988fb807821dd1a046f776dec))
* **init.el:** add function to sort words on line ([fab5007](https://github.com/PeterMosmans/dotfiles/commit/fab500753f63c06e0daea3552102ba094b5d9eef))
* **init.el:** add jinja2 package to parse Jinja templates ([058f019](https://github.com/PeterMosmans/dotfiles/commit/058f019ef0a5dfa4b63cc608aff65c6c123dcef7))
* **init.el:** add keybinding for helm-ag ([6e2efc4](https://github.com/PeterMosmans/dotfiles/commit/6e2efc423508e00438a9088d7720c22f51fe008f))
* **init.el:** disable unused packages ([f08e697](https://github.com/PeterMosmans/dotfiles/commit/f08e6970abfb605a39dde5df0d807589008cecbe))
* **init.el:** improve bootstrapping use-package ([9ddd22c](https://github.com/PeterMosmans/dotfiles/commit/9ddd22cec7ae7cf804f6d04efb3dc2d3108f5802))
* **init.el:** remove startup time customizations ([c167e15](https://github.com/PeterMosmans/dotfiles/commit/c167e155a2b953abdf2fc0d495beab17053b3710))
* use newer use-package bootstrap routine ([7d16299](https://github.com/PeterMosmans/dotfiles/commit/7d16299e0fabcfe59e1ac4a5f34bb9272216ae2b))
* **init.el:** ensure tabbar when using windowed system ([015c99a](https://github.com/PeterMosmans/dotfiles/commit/015c99a4ebda0347616cc3d244ee722802ca1010))
* **init.el:** lazy load as much packages as possible ([58c7daa](https://github.com/PeterMosmans/dotfiles/commit/58c7daa885938c9872887c73a3be13c8e023128a))
* **init.el:** limit number of completions for company ([7f9bca9](https://github.com/PeterMosmans/dotfiles/commit/7f9bca9c175ed9bb4c7f2de2d060a5ff8a37d9c5))
* **init.el:** open compilation mode in its own frame ([5c2e618](https://github.com/PeterMosmans/dotfiles/commit/5c2e618beb087a3ac97220867803d421b1802aec))
* **init.el:** replace rst-lint with doc8 ([3e5e76f](https://github.com/PeterMosmans/dotfiles/commit/3e5e76ff4137eb084637b5fcd564a11c93186490))
* **init.el:** switch to melpa as default package manager ([b4cbe1c](https://github.com/PeterMosmans/dotfiles/commit/b4cbe1c5aaf706d1cfdd01492b65ba0dd04969c1))
* **init.el:** use doc8 configuration if it can be found ([ecb3575](https://github.com/PeterMosmans/dotfiles/commit/ecb3575d19b1247d0eb445ef8a3d8c27d41df821))
* **init.el:** use Noto Emoji font for Unicode fonts ([d151450](https://github.com/PeterMosmans/dotfiles/commit/d151450d1a1d9daf30b15c1f3b03b67806bf252c))
* **installer:** enable dynamic creation of aliases ([901a552](https://github.com/PeterMosmans/dotfiles/commit/901a55249c3950b523073a3fb30c5d124b46e2f8))
* **installer:** update installer script ([40ecaa5](https://github.com/PeterMosmans/dotfiles/commit/40ecaa5be3c0652b997af45949726a597de57c03))
* **installer:** use generic installation script ([c2ac24a](https://github.com/PeterMosmans/dotfiles/commit/c2ac24a81013b3b2a9f77333a5b8362e8805e919))
* **zplug:** add list of plugins for OS ([c3045a1](https://github.com/PeterMosmans/dotfiles/commit/c3045a1fca595595aefb4f4b006eefb099d12940))
* **zplug:** add NO_SUGGESTIONS variable to disable autosuggestions ([adcff93](https://github.com/PeterMosmans/dotfiles/commit/adcff939c9205d800cef533033f8e69f4bf398b0))
* **zplug:** disable conditional loading of certain plugins ([28164d4](https://github.com/PeterMosmans/dotfiles/commit/28164d4896751a7541a2c877753bc12541051fca))
* **zplug:** ensure that tmux gets (not) started ([d631d58](https://github.com/PeterMosmans/dotfiles/commit/d631d5835e30204291c7ff2e51f91d1145d3c829))
* **zplug:** make zshrc environment independent ([81f39ce](https://github.com/PeterMosmans/dotfiles/commit/81f39ce8693bd32447d5f0e368d5962138b80afd))
* **zplug:** move zplug plugins to separate file ([299423f](https://github.com/PeterMosmans/dotfiles/commit/299423f900fad7fc480379d9936a26e8d08bedf3))
* **zsh:** move zsh aliases to .zshaliases file ([aa0a408](https://github.com/PeterMosmans/dotfiles/commit/aa0a408f224cab8e3ad31b4160097d9bf43e64ff))
* **zsh:** remove hashes and move zsh aliases ([c1e99fc](https://github.com/PeterMosmans/dotfiles/commit/c1e99fca8fd6bc55b10fa16d46ef8bd3f525cfbf))
* add ~/.local/bin to path when it exists ([7e75548](https://github.com/PeterMosmans/dotfiles/commit/7e75548c8ac997062f75996efd841dc458554707))
* improve robustness by using several double quotes ([ba5e25b](https://github.com/PeterMosmans/dotfiles/commit/ba5e25bdba26e1b7a4923e84579afec40f3d2607))
* remove zsh pip plugin as it sometimes hangs during install ([1eb8fc2](https://github.com/PeterMosmans/dotfiles/commit/1eb8fc263051a66f5eaf41b462dff9a5e734e288))
* split .zshenv into OS-dependent versions ([e58e808](https://github.com/PeterMosmans/dotfiles/commit/e58e80879b0138c934bbe670aa0e96a6bc466297))


### Bug Fixes

* ensure that file exists before trying to lint it ([1c26966](https://github.com/PeterMosmans/dotfiles/commit/1c26966f8f9a6c3ca132429c7be0a964deb3d380))
* ensure that prettier is only used when the binary is available ([56306c1](https://github.com/PeterMosmans/dotfiles/commit/56306c12ff4dfa49446ab7e94876f2a0cedd75d8))
* export LS colors ([297afb7](https://github.com/PeterMosmans/dotfiles/commit/297afb74ff3e49c143eb05bc54297da3b3cd75d4))
* **zplug:** let zsh plugins be set by env file ([9faf55e](https://github.com/PeterMosmans/dotfiles/commit/9faf55e7cdda134eac2a6cae5ccfbf41c6edd6ba))
* remove magit-gitflow package ([d6df413](https://github.com/PeterMosmans/dotfiles/commit/d6df41325e918af4ec599199ed05166bddd57d5e))
* remove self-manage option to increase stability ([30b1079](https://github.com/PeterMosmans/dotfiles/commit/30b10792c428349e0170af2e6d4ee036be6cc15e))
* sh has been renamed to shell in org-mode ([eacd973](https://github.com/PeterMosmans/dotfiles/commit/eacd97372ec3c896ad990d7d3c40590246ce0804))
