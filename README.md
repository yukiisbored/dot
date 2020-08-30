# yuki/dot

These are my personal dotfiles handled with GNU stow for easy symlink
management.

## Stuff that I use

| Function             | Software                                       |
|----------------------|------------------------------------------------|
| Operating systems    | OpenBSD, Windows 10, Manjaro Linux             |
| Display server       | X11 (Fuck wayland shills)                      |
| Window Manager       | i3                                             |
| Font                 | IBM Plex Sans (GUI) / IBM Plex Mono (Terminal) |
| Editor               | GNU Emacs                                      |
| Web browser          | Iridium / Chromium                             |
| Shell                | zsh                                            |
| File synchronisation | Nextcloud                                      |

## Questions that nobody asks

### Why 3 operating systems?

Windows 10 because games. I still *need* to play games and gaming on
Linux, while works, still sucks. Good luck getting Japanese region
titles running well in Wine among our lord and saviour, DRM.

Manjaro Linux because I have a Pinebook Pro. While OpenBSD runs, it's
still lacking a lot of stuff that I would consider essential for me
to be able to daily drive it. Even then, school requires me running
software like Android Studio. So yeah...

OpenBSD because I love my sanity so much that I rather not deal with
the pains and trauma events running Linux on my *personal* machines
without a good reason.

## Why do you care about running OpenBSD?

**Fuck Linux.**

I hate it so much and this is me, someone who works as a "DevOps
Engineer" and System administrator who deals with it on a daily basis
and at a professional capacity and at a huge scale.

It has caused me so many fucking pain and nothing *but* pain for no
god damn reason, such as but not limited to:

* Point releases breaking random shit for no god damn reason.
* Replacing existing system components and tools without a good reason
  instead of just improving them.
* General toxic attitude in the community.
  * Useless distro wars anyone?
  * ... and now yet another war on package managers / app
    distributions
  * Oh did I also mention container politics? yeaaaah....
* Mixed quality of various components in the kernel.
  * Usually caused by ignorant vendors who just want a quick easy way
    to get their shit to run without doing proper support nor
    following guidelines and just end up reinventing shit for no god
    damn reason.
    * I fucking see you, Broadcom, Realtek and AMD.
* IPTables.
  * Who the fuck thought this shit is a good idea?
    * Like no, seriously.

In comparison, OpenBSD has been generally tame and I have a much
*much* **much** better experience using it as a daily driver that
retains my general sanity and temper.

While people, some people, might argue that OpenBSD security practices
are a bit excessive or aren't backed by proper academic research.

Here's my opinion: **I don't give a fuck**.

OpenBSD has a better and friendlier community with developers that
actually eat their own dogfood everyday.

Security is literallythe *last* reasons why I love OpenBSD over other
UNIX-like operating systems.

It has better userland utilities that's sane backed with one of the
best documentation ever that I can easily skim through without having
to go through a gazillion random forum sites.

Doing 99% of things within it (excluding ports) is just so fucking
simple and easy to the point that I can be so fucking high and drunk
at the same time and I wouldn't have any god damn issues setting up a
router with CARP and fail-over.

I've been daily driving CURRENT (the development branch) on my
Thinkpad X220 that I use for university and I've had 0 problems with
it. Absolutely nothing.

My only issue with it is the filesystem, FFS2, is kinda old when
comparing to ZFS but if I need an actual storage server, I can just
have a server running HardenedBSD. Having filesystem snapshots along
with the ability to switch to a particular snapshot from the
bootloader would be really nice though.

## Why not Wayland because Wayland is better for touch device and blahblah...?

1. Wayland isn't a thing in BSDs.
2. Shut the fuck up for fuck's sake, I'd rather have something that
   can run all of my software rather than breaking 10% of it.

## Emacs, seriously?

It just works, I don't give a fuck. You can laugh at the fact it's a
monolithic operating system or takes 30 minutes to load or the fact it
takes only (*sees top*) 120 MB of RAM.

It works fine for me and I can do shit from writing blog posts to read
emails to writing Ansible Playbooks without a hitch.

## Mirrors

This repository is hosted on https://git.yukiisbo.red/yuki/dot

Mirrors are setup on both [GitHub.com][gh-mirror] and [GitLab.com][gl-mirror].

[gh-mirror]: https://github.com/yukiisbored/dot
[gl-mirror]: https://gitlab.com/yuki_is_bored/dot
