# Gold-Level SPARK Proofs

This document describes the proof strategy used to achieve near-Gold-level SPARK verification for the hadlink core module.

For background on SPARK verification methodology, see the [Implementation Guidance for the Adoption of SPARK](https://www.adacore.com/uploads/books/Spark-Guidance-1.2-web.pdf) (AdaCore & Thales).

## Contents

- [Background](#background)
- [The Problem](#the-problem)
- [Approaches Considered](#approaches-considered)
- [Solution: Unified Functions with Ghost Lemma](#solution-unified-functions-with-ghost-lemma)
- [Proof Chain](#proof-chain)
- [Results](#results)
- [Remaining Assumes](#remaining-assumes)
- [Related Documentation](#related-documentation)

---

## Background

SPARK verification levels (per AdaCore) progress from Stone to Platinum:

| Level | Description |
|-------|-------------|
| Stone | Valid SPARK code, compiles |
| Bronze | Flow analysis passes |
| Silver | AoRTE (Absence of Run-Time Errors) proven |
| Gold | Key properties proven without `pragma Assume` |
| Platinum | Full functional correctness proven |

The hadlink core previously used 3 `pragma Assume` statements in `Canonicalize` to bridge the gap between validation checks and postcondition properties. This document describes the refactoring that reduced these to 2 assumes, confined them to a documented ghost lemma, and made the business logic assume-free.

---

## The Problem

The `Canonicalize` function validates a URL and returns a `Valid_URL` with security properties guaranteed by its postcondition:

```ada
Post => (if Canonicalize'Result.Status = Success
         then Is_HTTP_Or_HTTPS (Canonicalize'Result.URL) and then
              Not_Private_Address (Canonicalize'Result.URL) and then
              No_Credentials (Canonicalize'Result.URL));
```

The implementation validates these properties on the input string, then copies the input into the result. The prover needs to understand that properties verified on the input also hold on the output.

### Original Approach (3 assumes in business logic)

```ada
--  In Canonicalize, after validation:
pragma Assume (Is_HTTP_Or_HTTPS (Result.URL),
               "Has_Valid_Scheme verified http(s):// prefix...");
pragma Assume (Not_Private_Address (Result.URL),
               "Is_Private_IP verified host is not private...");
pragma Assume (No_Credentials (Result.URL),
               "Has_Credentials verified no credentials...");
```

These assumes were scattered in the business logic and required auditors to trust that the validation functions and query functions performed equivalent checks.

---

## Approaches Considered

### Type_Invariant Approach

The most elegant solution would encode security properties directly into the type:

```ada
type Valid_URL is private with
   Type_Invariant => Valid_URL_Properties (Valid_URL);

function Valid_URL_Properties (URL : Valid_URL) return Boolean is
  (Has_Valid_Scheme (To_String (URL)) and then
   not Has_Credentials (To_String (URL)) and then
   not Has_Private_Host (To_String (URL)));
```

**Result**: Blocked by SPARK RM 7.3.2(2), which prohibits `Type_Invariant` on private types:

```
core.ads:62:09: error: type invariant on private_type_declaration or
private_type_extension is not allowed in SPARK (SPARK RM 7.3.2(2))
```

### Ghost Lemma Approach (Implemented)

Unify the validation and query functions so they call the same predicates, then use a ghost lemma to help the prover understand that equal strings produce equal predicate results.

---

## Solution: Unified Functions with Ghost Lemma

### Step 1: Shared Predicates

Define validation predicates that are used by both `Canonicalize` (for validation) and the query functions (for postcondition proofs):

```ada
--  Check if string has valid HTTP/HTTPS scheme
function Has_Valid_Scheme (S : String) return Boolean is
  ((S'Length >= 8 and then S (S'First .. S'First + 7) = "https://")
   or else
   (S'Length >= 7 and then S (S'First .. S'First + 6) = "http://"));

--  Check if string contains credentials (user:pass@)
function Has_Credentials (S : String) return Boolean;

--  Check if host portion is a private IP address
function Has_Private_Host (S : String) return Boolean;
```

### Step 2: Expression Function Query Functions

Define query functions as expression functions that call the shared predicates:

```ada
function Is_HTTP_Or_HTTPS (URL : Valid_URL) return Boolean is
  (Has_Valid_Scheme (To_String (URL)));

function Not_Private_Address (URL : Valid_URL) return Boolean is
  (not Has_Private_Host (To_String (URL)));

function No_Credentials (URL : Valid_URL) return Boolean is
  (not Has_Credentials (To_String (URL)));
```

### Step 3: Make_Valid_URL with Content Postcondition

Add a constructor function that guarantees content equality:

```ada
function Make_Valid_URL (S : String) return Valid_URL
with
  Pre  => S'Length >= 7 and then
          S'Length <= Max_URL_Length and then
          S'First = 1,
  Post => To_String (Make_Valid_URL'Result) = S and then
          Length (Make_Valid_URL'Result) = S'Length;
```

### Step 4: Ghost Lemma for Predicate Substitution

The prover cannot automatically deduce that `f(A) = f(B)` when `A = B` for non-expression functions. We provide a ghost lemma:

```ada
procedure Lemma_Predicate_Substitution
  (A : String;
   B : String)
with
  Ghost,
  Pre  => A = B and then A'Length >= 7 and then ...,
  Post => Has_Valid_Scheme (A) = Has_Valid_Scheme (B) and then
          Has_Credentials (A) = Has_Credentials (B) and then
          Has_Private_Host (A) = Has_Private_Host (B)
is
begin
   --  Has_Valid_Scheme is an expression function, proves automatically.
   --  Has_Credentials and Has_Private_Host have bodies, require assumes.
   pragma Assume (Has_Credentials (A) = Has_Credentials (B),
                  "Pure function determinism: A = B implies f(A) = f(B)");
   pragma Assume (Has_Private_Host (A) = Has_Private_Host (B),
                  "Pure function determinism: A = B implies f(A) = f(B)");
end Lemma_Predicate_Substitution;
```

### Step 5: Updated Canonicalize

```ada
function Canonicalize (Input : String) return Canonicalize_Result is
   ...
begin
   --  Validation checks establish:
   --    Has_Valid_Scheme(Input) = True
   --    Has_Credentials(Input) = False
   --    Has_Private_Host(Input) = False

   Result.URL := Make_Valid_URL (Input);
   Result.Status := Success;

   --  Apply the substitution lemma
   Lemma_Predicate_Substitution (To_String (Result.URL), Input);

   --  Now provable by substitution
   pragma Assert (Has_Valid_Scheme (To_String (Result.URL)));
   pragma Assert (not Has_Credentials (To_String (Result.URL)));
   pragma Assert (not Has_Private_Host (To_String (Result.URL)));

   return Result;
end Canonicalize;
```

---

## Proof Chain

The complete proof chain for the postcondition:

1. **Canonicalize validates input**:
   - `Has_Valid_Scheme(Input) = True`
   - `Has_Credentials(Input) = False`
   - `Has_Private_Host(Input) = False`

2. **Make_Valid_URL guarantees content equality**:
   - `To_String(Result.URL) = Input`

3. **Ghost lemma establishes predicate equality**:
   - `Has_Valid_Scheme(To_String(Result.URL)) = Has_Valid_Scheme(Input)`
   - `Has_Credentials(To_String(Result.URL)) = Has_Credentials(Input)`
   - `Has_Private_Host(To_String(Result.URL)) = Has_Private_Host(Input)`

4. **Query functions are expression functions**:
   - `Is_HTTP_Or_HTTPS(URL) = Has_Valid_Scheme(To_String(URL))`
   - `No_Credentials(URL) = not Has_Credentials(To_String(URL))`
   - `Not_Private_Address(URL) = not Has_Private_Host(To_String(URL))`

5. **By substitution**:
   - `Is_HTTP_Or_HTTPS(Result.URL) = Has_Valid_Scheme(Input) = True`
   - `No_Credentials(Result.URL) = not Has_Credentials(Input) = True`
   - `Not_Private_Address(Result.URL) = not Has_Private_Host(Input) = True`

6. **Postcondition satisfied. QED.**

---

## Results

| Metric | Before | After |
|--------|--------|-------|
| `pragma Assume` in Canonicalize | 3 | 0 |
| `pragma Assume` in ghost lemma | 0 | 2 |
| **Total assumes** | 3 | 2 |
| Assumes in business logic | Yes | No |
| Assumes documented/confined | No | Yes |

### Benefits

1. **Reduced assume count**: 3 → 2
2. **Assumes confined to ghost lemma**: Business logic is assume-free
3. **Better justification**: Assumes are about pure function determinism, a mathematical truth
4. **Auditable**: Single location to review, clear documentation
5. **Maintainable**: Changes to validation don't require updating scattered assumes

---

## Remaining Assumes

The 2 remaining assumes exist because `Has_Credentials` and `Has_Private_Host` are not expression functions. Their implementations involve loops and complex logic that cannot be expressed as expression functions.

The assumes state a mathematical truth: pure functions are deterministic. When `A = B`, it is certain that `f(A) = f(B)` for any pure function `f`. The prover cannot automatically deduce this for non-expression functions because it would create an explosion of proof obligations for all possible function calls.

### Alternative: Expression Functions

To eliminate these assumes entirely, `Has_Credentials` and `Has_Private_Host` would need to be rewritten as expression functions. This is theoretically possible but would require:

1. Replacing loops with recursive expression functions
2. Adding termination proofs for the recursion
3. Potentially restructuring the validation logic

This trade-off was considered too complex for the benefit, especially since the remaining assumes are mathematically sound and clearly documented.

---

## Related Documentation

- [FFI_INTEGRATION.md](FFI_INTEGRATION.md) - FFI boundary design
- [ROADMAP.md](ROADMAP.md) - DO-278A SIL-3 mapping
- [README.md](../README.md) - Project overview
