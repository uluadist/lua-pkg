# lua-pkg
Universal Lua(JIT) Package Manager (**Preliminary documentation**)

In short: simple to integrate, multiple versions, multiple OS and archs, no compiling needed, completely updateable, around 300 packages available.

This beta release covers Windows-x86 only, to try it download and unpack:
http://scilua.org/files/core.zip

Lua is run via lua.cmd and the package manager is loaded via:
`local pkg = require 'pkg'`

PKG - the package manager - main points:
----------------------------------------

* 1 KLOC Lua + lfs + curl + zip
* 1-package <-> 1-module <-> 1-directory (module name == package name)
* no package database
* meta information: `<module_name>/__meta.lua` is everything needed to integrate the package manager (jointly with naming conventions for OS-dependent components: Lua C modules, dynamic libraries, ...)
* version format: `'%d+%.?%d*%.?%d*%.?%a*%d*%-?%d*'`, `%a` one of 
  `{ 
    head  = 1,
    alpha = 2,
    beta  = 3,
    work  = 4,
    rc    = 5,
    ['']  = 6, --[[Stable]]
    patch = 7, --[[Stable]]
  } --[[Case insensitive matching]]`
* semantic versioning
* multiple major versions are allowed (right one is required looking at `__meta.lua`) but no others
* relative directories and uniform behavior across OSes: installation can be moved around and accessed by multiple OSes
* support for { Windows, Linux, OSX } x { x86, x64 } in each package [Windows-x86 only in beta1]
* support for embedded C-libs (dynamic libraries needed by the module)
* both the package manager and the LuaJIT binaries are updateable via the package manager itself
* support for proxy (with authentication)

PKG - API:
----------
```lua
local pkg = require 'pkg'

pkg.status('?') -- Info about all installed packages.
pkg.status('?curl') -- Info about installed packages somehow matching 'curl'.
pkg.status('lfs') -- Detailed info about installed LuaFileSystem.
```

`pkg.available()` is same as `pkg.status()` for available packages (at the online official repository: scilua.org/pkg).

```lua
pkg.add('pl') -- Install penlight and its dependencies.
pkg.remove('pl') -- Remove penlight and packages that depend on it.
```

It is possible to specify a version (string) as second argument to pkg.add() 
and pkg.remove(). The found version will be added / removed where found means:
  + same major version
  + version constraint satisfied
  + most recent stable, or most recent unstable if no stable found

```lua
pkg.update() -- Update all packages.
```

`host/config.lua` -- User settings: `proxy`, `proxyauth`, `silent`, `noconfirm`.

All functions accept a trailing table argument which overrides the settings of `config.lua`.

PKG - LUAROCK:
--------------

* auto-build from rockspecs in Luarock => universal Lua packages
* multiple modules inside a single rock => split
* around 300 packages available right now
* failures:  
  + same module in multiple rocks => conflict
  + bad version formats (initial 'v' or 'r' is ignored)
  + external dependencies (libraries) are not supported yet
  + some rocks are not available for all OSes
  + various compile / download errors
