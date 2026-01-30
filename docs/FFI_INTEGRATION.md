# FFI Integration Technical Notes

## Contents

- [Current Status](#current-status)
- [Historical Context: The Problem](#historical-context-the-problem-solved)
- [Root Cause](#root-cause-historical)
- [Attempted Solutions](#attempted-solutions)
- [Proper Solutions](#proper-solutions)
- [Solution Implemented](#solution-implemented)
- [References](#references)
- [Files Modified for FFI](#files-modified-for-ffi)
- [Testing](#testing)

---

## Current Status

[x] **Phase 2.5 FFI integration is COMPLETE!**

**Solution**: GNAT Standalone Library with encapsulated Ada runtime.

### What Works

- SPARK core compiles to shared library: `spark-core/lib/libHadlink_Core.so`
- Library built as standalone encapsulated (Ada runtime included)
- Exported C-compatible functions:
  - `hadlink_init` - Ada runtime initialization
  - `hadlink_final` - Ada runtime finalization
  - `hadlink_canonicalize` - URL validation and canonicalization
  - `hadlink_make_short_code` - Deterministic short code generation
- Haskell FFI module `SparkFFI.hs` with proper bindings
- Haskell code uses IO-based FFI calls
- Full HTTP daemon working with SPARK backend
- 17 property tests pass (14 via FFI, 3 rate limiting)

### Important: Single-Threaded FFI

The Ada runtime is **not thread-safe** for concurrent FFI calls. Tests and applications must use single-threaded execution when calling SPARK functions:

```bash
# Tests run with single RTS thread
hadlink-test +RTS -N1 -RTS
```

The test suite enforces this via `localOption (NumThreads 1)` in Tasty configuration.

### Historical Context: The Problem (Solved)

Initially, the Haskell executable segfaulted when calling SPARK FFI functions:

```
$ ./test-ffi
Segmentation fault (core dumped)
```

### Root Cause (Historical)

Ada libraries require complex runtime initialization:

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

### Solution Implemented

**Standalone Library** - Successful

#### What Worked:

1. **GNAT Standalone Library Configuration**:
   ```ada
   for Library_Kind use "relocatable";  -- Must be shared library
   for Library_Standalone use "encapsulated";  -- Ada runtime included
   for Library_Interface use ("Core_FFI");  -- Export this package
   ```

2. **Ada Runtime Auto-Initialization**: Standalone library handles `adainit`/`adafinal` automatically when loaded

3. **Fixed FFI Buffer Management**: Changed `Update(..., Check => True)` to `Update(..., Check => False)` to avoid Ada string validation on Haskell-allocated buffers

4. **No Manual Init Required**: Removed calls to `hadlink_init` - standalone library handles initialization

#### Results:
- [x] FFI calls work without segfaults
- [x] URL canonicalization: `https://example.com/test` → success
- [x] Short code generation: produces valid 8-character codes
- [x] Full HTTP daemon integration working
- [x] Haskell → SPARK → Haskell round trip complete

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

Property tests exercise the FFI:

```bash
redo test   # Runs 14 Hedgehog property tests via SPARK FFI
```

Test output:
```
All 17 tests passed (0.26s)
```

For manual FFI testing:
```bash
redo run-shorten  # Starts HTTP server with SPARK backend
curl -X POST http://localhost:8080/api/create -d "url=https://example.com"
```
