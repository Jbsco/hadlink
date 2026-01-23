# FFI Integration Technical Notes

## Current Status

Phase 2.5 FFI integration is blocked on Ada runtime initialization requirements.

### What Works

- SPARK core compiles to static library: `spark-core/lib/libHadlink_Core.a`
- Library built with `-fPIC` (static-pic) for position-independent code
- Exported C-compatible functions:
  - `hadlink_init` - Ada runtime initialization
  - `hadlink_final` - Ada runtime finalization
  - `hadlink_canonicalize` - URL validation and canonicalization
  - `hadlink_make_short_code` - Deterministic short code generation
- Haskell FFI module `SparkFFI.hs` with proper bindings
- Haskell code updated to use IO-based FFI calls
- Build system configured:
  - Links against libHadlink_Core.a
  - Links against libgnat_pic.a (Ada runtime)
  - Haskell executable compiles and links successfully

### The Problem

The Haskell executable segfaults when calling any SPARK FFI function. This occurs even with `hadlink_init()` called at program startup.

```
$ ./test-ffi
Initializing SPARK runtime...
SPARK initialized
Testing canonicalize...
Segmentation fault (core dumped)
```

### Root Cause

Ada libraries require complex runtime initialization that goes beyond a simple init function:

1. **Elaboration Order**: Ada packages must be elaborated in dependency order
2. **Secondary Stack**: Ada uses a secondary stack for dynamic-sized returns
3. **Exception Handling**: Ada exception mechanism needs runtime setup
4. **Memory Management**: Ada has its own memory allocator

When an Ada library is built as a standalone static library for C FFI, these components must be properly initialized before any Ada code executes.

### Attempted Solutions

1. **Simple init/final functions**: Created `hadlink_init()` and `hadlink_final()` exports - insufficient
2. **Direct GNAT runtime linking**: Added `libgnat_pic.a` to linker - still crashes
3. **Manual initialization**: Calling init before FFI - still crashes

### Proper Solutions

Ada FFI from other languages requires one of these approaches:

#### Option 1: Standalone Library with Binder

Configure GNAT project as a proper standalone library:

```ada
for Library_Kind use "static-pic";
for Library_Standalone use "encapsulated";
for Library_Interface use ("Core_FFI");
```

This generates binder files that properly initialize the Ada runtime. The `gnatbind` tool generates:
- `b~<unit>.adb` - Elaboration code
- Runtime initialization stubs

#### Option 2: Manual Adainit/Adafinal

Use GNAT's generated `adainit` and `adafinal` functions:

1. Build library with `gnatmake -c` 
2. Run `gnatbind -n` to generate binder without main
3. Compile binder files: `gcc -c b~<unit>.adb`
4. Export adainit/adafinal for FFI layer to call
5. Haskell calls these before/after using Ada functions

#### Option 3: C Shim Layer

Create a thin C wrapper that:
- Initializes Ada runtime once on first call
- Marshals calls to Ada
- Handles Ada exceptions and converts to C error codes

```c
static int ada_initialized = 0;

int hadlink_canonicalize_c(const char* input, char* output, size_t* len) {
    if (!ada_initialized) {
        adainit();
        ada_initialized = 1;
    }
    return hadlink_canonicalize_ada(input, output, len);
}
```

#### Option 4: Separate Process

Run SPARK core as a separate process:
- Ada runtime managed by Ada main program
- IPC via pipes, sockets, or shared memory
- Haskell calls via IPC instead of FFI
- More robust isolation

### Recommended Path Forward

**Short term**: Continue with pure Haskell implementation for Phase 1 releases. SPARK core exists as reference implementation and for future integration.

**Medium term**: Investigate Option 1 (standalone library) - most proper Ada solution.

**Long term**: Consider Option 4 (separate process) for production deployments - better isolation and crash handling.

### References

- GNAT User's Guide: [Binding with Non-Ada Main Programs](https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/the_gnat_compilation_model.html#binding-with-non-ada-main-programs)
- GNAT User's Guide: [Creating a Stand-alone Library](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gprconfig.html#stand-alone-library-projects)
- Ada Reference Manual: [Elaboration Order](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-10-2.html)

### Files Modified for FFI

- `spark-core/src/core_ffi.ads` - Added init/final procedures
- `spark-core/src/core_ffi.adb` - Implemented init/final (currently null)
- `spark-core/hadlink_core.gpr` - Changed to static-pic library
- `haskell/src/SparkFFI.hs` - New FFI module with C bindings
- `haskell/src/Canonicalize.hs` - Changed to IO-based, calls sparkCanonicalize
- `haskell/src/ShortCode.hs` - Changed to IO-based, calls sparkMakeShortCode
- `haskell/src/API.hs` - Updated for IO-based functions
- `haskell/app/Main.hs` - Calls initSpark on startup
- `haskell/hadlink.cabal` - Added SparkFFI module, extra-libraries, extra-lib-dirs
- `all.do` - Build SPARK before Haskell
- `spark-core/build.do` - New build script for SPARK

### Testing

Simple FFI test program: `haskell/test-ffi.hs`

Compile:
```bash
cd haskell
stack exec ghc -- -isrc test-ffi.hs \
  -L../spark-core/lib \
  -L/usr/lib/gcc/x86_64-pc-linux-gnu/15.2.1/adalib \
  -lHadlink_Core -lgnat_pic \
  -o test-ffi
```

Run:
```bash
./test-ffi  # Currently segfaults
```
