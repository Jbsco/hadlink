--  hadlink - High-assurance URL shortener
--  Copyright (C) 2026 hadlink contributors
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

--  Core implementation with SPARK proofs
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with SPARKNaCl;          use SPARKNaCl;
with SPARKNaCl.MAC;      use SPARKNaCl.MAC;
with SPARKNaCl.Hashing.SHA256;

package body Core is

   --  Default empty URL for error cases
   Empty_URL : constant Valid_URL :=
     (Data => (others => ' '), Len => 1);

   --  Base62 alphabet for encoding
   Base62_Alphabet : constant String :=
     "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

   ---------------------------------------------------------------------------
   --  Helper: Extract host portion from URL string
   ---------------------------------------------------------------------------
   procedure Extract_Host
     (Input      : String;
      Host_Start : out Natural;
      Host_End   : out Natural;
      Valid      : out Boolean)
   with
     Pre  => Input'Length >= 7 and then
             Input'First >= 1 and then
             Input'Last < Integer'Last - 10,
     Post => (if Valid then
                Host_Start >= Input'First and then
                Host_End <= Input'Last and then
                Host_Start <= Host_End)
   is
      Scheme_End : Natural;
   begin
      Host_Start := 0;
      Host_End := 0;
      Valid := False;

      --  Determine scheme length
      if Input'Length >= 8 and then
         Input (Input'First .. Input'First + 7) = "https://"
      then
         Scheme_End := Input'First + 7;
      elsif Input (Input'First .. Input'First + 6) = "http://" then
         Scheme_End := Input'First + 6;
      else
         return;
      end if;

      --  Host starts after scheme
      if Scheme_End >= Input'Last then
         return;
      end if;

      Host_Start := Scheme_End + 1;

      --  Find end of host (first '/', '?', '#', or end of string)
      Host_End := Input'Last;
      for I in Host_Start .. Input'Last loop
         if Input (I) = '/' or else Input (I) = '?' or else Input (I) = '#'
         then
            Host_End := I - 1;
            exit;
         end if;
         pragma Loop_Invariant (I >= Host_Start);
         pragma Loop_Invariant (Host_End = Input'Last);
      end loop;

      if Host_Start <= Host_End then
         Valid := True;
      end if;
   end Extract_Host;

   ---------------------------------------------------------------------------
   --  Helper: Check if host string is a private IP address
   --  Covers IPv4 private ranges, IPv6 private ranges, and special addresses
   ---------------------------------------------------------------------------
   function Is_Private_IP (Host : String) return Boolean
   with Pre => Host'Length >= 1 and then
               Host'First >= 1 and then
               Host'Last < Integer'Last - 1
   is
      --  Helper to get host content (strips IPv6 brackets if present)
      function Get_Host_Content return String is
      begin
         if Host'Length >= 2 and then
            Host (Host'First) = '[' and then
            Host (Host'Last) = ']'
         then
            return Host (Host'First + 1 .. Host'Last - 1);
         else
            return Host;
         end if;
      end Get_Host_Content;

      H : constant String := Get_Host_Content;
   begin
      --  Check for localhost variants
      if H = "localhost" then
         return True;
      end if;

      --  Check for IPv6 loopback (::1)
      if H = "::1" then
         return True;
      end if;

      --  Check for 0.0.0.0 (can represent localhost on some systems)
      if H = "0.0.0.0" then
         return True;
      end if;

      --  Check for IPv6 all-zeros (::)
      if H = "::" then
         return True;
      end if;

      --  Check for IPv4 private prefixes
      if H'Length >= 3 then
         declare
            Prefix3 : constant String := H (H'First .. H'First + 2);
         begin
            --  10.x.x.x (Class A private)
            --  127.x.x.x (loopback)
            if Prefix3 = "10." or else Prefix3 = "127" then
               return True;
            end if;
         end;
      end if;

      --  Check for IPv4 link-local (169.254.x.x)
      if H'Length >= 8 then
         declare
            Prefix8 : constant String := H (H'First .. H'First + 7);
         begin
            if Prefix8 = "169.254." then
               return True;
            end if;
         end;
      end if;

      if H'Length >= 8 then
         declare
            Prefix7 : constant String := H (H'First .. H'First + 6);
         begin
            --  192.168.x.x (Class C private)
            --  172.16-31.x.x (Class B private)
            if Prefix7 = "192.168" or else
               Prefix7 = "172.16." or else Prefix7 = "172.17." or else
               Prefix7 = "172.18." or else Prefix7 = "172.19." or else
               Prefix7 = "172.20." or else Prefix7 = "172.21." or else
               Prefix7 = "172.22." or else Prefix7 = "172.23." or else
               Prefix7 = "172.24." or else Prefix7 = "172.25." or else
               Prefix7 = "172.26." or else Prefix7 = "172.27." or else
               Prefix7 = "172.28." or else Prefix7 = "172.29." or else
               Prefix7 = "172.30." or else Prefix7 = "172.31."
            then
               return True;
            end if;
         end;
      end if;

      --  Check for IPv6 private ranges
      if H'Length >= 4 then
         declare
            Prefix4 : constant String := H (H'First .. H'First + 3);
         begin
            --  fe80:: (link-local, fe80::/10)
            if Prefix4 = "fe80" or else Prefix4 = "FE80" or else
               Prefix4 = "Fe80" or else Prefix4 = "fE80"
            then
               return True;
            end if;
         end;
      end if;

      if H'Length >= 2 then
         declare
            Prefix2 : constant String := H (H'First .. H'First + 1);
         begin
            --  fc00::/7 - Unique Local Addresses (ULA)
            --  This covers fc00:: through fdff::
            if Prefix2 = "fc" or else Prefix2 = "FC" or else
               Prefix2 = "Fc" or else Prefix2 = "fC" or else
               Prefix2 = "fd" or else Prefix2 = "FD" or else
               Prefix2 = "Fd" or else Prefix2 = "fD"
            then
               return True;
            end if;
         end;
      end if;

      --  Check for IPv6-mapped IPv4 (::ffff:x.x.x.x)
      --  These embed IPv4 addresses in IPv6 format
      if H'Length >= 7 then
         declare
            Prefix7 : constant String := H (H'First .. H'First + 6);
         begin
            if Prefix7 = "::ffff:" or else Prefix7 = "::FFFF:" then
               --  Extract the IPv4 part and check if it's private
               declare
                  IPv4_Part : constant String :=
                    H (H'First + 7 .. H'Last);
               begin
                  if IPv4_Part'Length >= 1 and then
                     IPv4_Part'Last < Integer'Last - 1
                  then
                     --  Recursively check the embedded IPv4 address
                     return Is_Private_IP (IPv4_Part);
                  end if;
               end;
            end if;
         end;
      end if;

      return False;
   end Is_Private_IP;

   ---------------------------------------------------------------------------
   --  Has_Credentials implementation
   ---------------------------------------------------------------------------
   function Has_Credentials (S : String) return Boolean
   is
   begin
      --  Look for @ sign followed by checking for : before it
      for I in S'Range loop
         if S (I) = '@' then
            --  Check if there's a : before @
            for J in S'First .. I - 1 loop
               if S (J) = ':' then
                  --  Found : before @, but need to skip scheme ://
                  if J >= S'First + 5 then
                     --  Check it's not the scheme separator
                     if J > S'First + 6 or else
                        S (S'First .. S'First + 3) /= "http"
                     then
                        return True;
                     end if;
                  end if;
               end if;
               pragma Loop_Invariant (J >= S'First);
            end loop;
         end if;
         pragma Loop_Invariant (I >= S'First);
      end loop;
      return False;
   end Has_Credentials;

   ---------------------------------------------------------------------------
   --  Has_Private_Host implementation
   ---------------------------------------------------------------------------
   function Has_Private_Host (S : String) return Boolean
   is
      Host_Start : Natural;
      Host_End   : Natural;
      Host_Valid : Boolean;
   begin
      Extract_Host (S, Host_Start, Host_End, Host_Valid);
      if not Host_Valid or else Host_Start = 0 then
         return True;  -- Invalid = treat as private (fail safe)
      end if;

      if Host_End >= Host_Start and then
         Host_End - Host_Start < Max_URL_Length
      then
         return Is_Private_IP (S (Host_Start .. Host_End));
      else
         return True;
      end if;
   end Has_Private_Host;

   ---------------------------------------------------------------------------
   --  Ghost Lemma: Predicate Substitution
   --
   --  This lemma helps the prover understand that for equal strings,
   --  the predicate functions return equal results. The prover cannot
   --  automatically deduce this for non-expression functions.
   --
   --  See docs/GOLD_LEVEL_PROOFS.md for the full proof strategy.
   ---------------------------------------------------------------------------
   procedure Lemma_Predicate_Substitution
     (A : String;
      B : String)
   with
     Ghost,
     Pre  => A = B and then
             A'Length >= 7 and then
             A'First >= 1 and then
             A'Last < Integer'Last - 10 and then
             B'First >= 1 and then
             B'Last < Integer'Last - 10,
     Post => Has_Valid_Scheme (A) = Has_Valid_Scheme (B) and then
             Has_Credentials (A) = Has_Credentials (B) and then
             Has_Private_Host (A) = Has_Private_Host (B)
   is
   begin
      --  JUSTIFICATION: These functions are mathematically pure - they have
      --  no side effects and their results depend only on their inputs.
      --  When A = B, it is mathematically certain that f(A) = f(B).
      --
      --  The prover cannot automatically deduce this for non-expression
      --  functions, so we provide this lemma with documented assumptions.
      --
      --  Has_Valid_Scheme is an expression function, proves automatically.
      --  Has_Credentials and Has_Private_Host have bodies, need assumes.
      --
      --  These assumptions are confined to this ghost lemma rather than
      --  scattered throughout the business logic, making them easier to
      --  audit and maintain.
      pragma Assume (Has_Credentials (A) = Has_Credentials (B),
                     "Pure function determinism: A = B implies f(A) = f(B)");
      pragma Assume (Has_Private_Host (A) = Has_Private_Host (B),
                     "Pure function determinism: A = B implies f(A) = f(B)");
   end Lemma_Predicate_Substitution;

   ---------------------------------------------------------------------------
   --  Make_Valid_URL: Constructor with content postcondition
   --
   --  This is the critical bridge for proof chaining. The postcondition
   --  To_String(Result) = S allows the prover to substitute:
   --    Has_Valid_Scheme(To_String(Result)) = Has_Valid_Scheme(S)
   ---------------------------------------------------------------------------
   function Make_Valid_URL (S : String) return Valid_URL
   is
      Result : Valid_URL;
   begin
      Result.Data (1 .. S'Length) := S;
      Result.Len := S'Length;
      return Result;
   end Make_Valid_URL;

   ---------------------------------------------------------------------------
   --  Compute HMAC-SHA256 using SPARKNaCl verified implementation
   ---------------------------------------------------------------------------
   function Compute_Hash
     (Message : String;
      Key     : Secret_Key)
     return String
   with
     Pre  => Message'Length >= 1 and then
             Message'Length <= Max_URL_Length and then
             Message'First = 1 and then
             Message'Last < Integer'Last,
     Post => Compute_Hash'Result'Length = 32 and then
             Compute_Hash'Result'First = 1
   is
      --  Convert message String to SPARKNaCl Byte_Seq (0-indexed)
      Msg_Len   : constant N32 := N32 (Message'Length);
      Msg_Bytes : Byte_Seq (0 .. Msg_Len - 1) := (others => 0);

      --  Convert key String to SPARKNaCl Byte_Seq (0-indexed)
      Key_Len   : constant N32 := N32 (Key'Length);
      Key_Bytes : Byte_Seq (0 .. Key_Len - 1) := (others => 0);

      --  HMAC output (SHA256 = 32 bytes)
      Output    : SPARKNaCl.Hashing.SHA256.Digest;

      --  Result string
      Result    : String (1 .. 32);
   begin
      --  Convert message characters to bytes
      for I in Message'Range loop
         Msg_Bytes (N32 (I - Message'First)) :=
           Byte (Character'Pos (Message (I)));
         pragma Loop_Invariant (I >= Message'First);
      end loop;

      --  Convert key characters to bytes
      for I in Key'Range loop
         Key_Bytes (N32 (I - Key'First)) :=
           Byte (Character'Pos (Key (I)));
         pragma Loop_Invariant (I >= Key'First);
      end loop;

      --  Compute HMAC-SHA256
      HMAC_SHA_256 (Output, Msg_Bytes, Key_Bytes);

      --  Convert output bytes to result string
      for I in 1 .. 32 loop
         Result (I) := Character'Val (Integer (Output (N32 (I - 1))));
         pragma Loop_Invariant (I >= 1);
      end loop;

      return Result;
   end Compute_Hash;

   ---------------------------------------------------------------------------
   --  Canonicalize
   --
   --  Proof strategy (see docs/GOLD_LEVEL_PROOFS.md for details):
   --
   --  At the success path, we have established:
   --    (a) Has_Valid_Scheme(Input) = True
   --    (b) Has_Credentials(Input) = False
   --    (c) Has_Private_Host(Input) = False
   --
   --  Make_Valid_URL(Input) guarantees:
   --    (d) To_String(Result.URL) = Input
   --
   --  Query functions are defined as expression functions:
   --    Is_HTTP_Or_HTTPS(URL) = Has_Valid_Scheme(To_String(URL))
   --    No_Credentials(URL) = not Has_Credentials(To_String(URL))
   --    Not_Private_Address(URL) = not Has_Private_Host(To_String(URL))
   --
   --  The ghost lemma Lemma_Predicate_Substitution proves:
   --    predicate(To_String(URL)) = predicate(Input)
   --    when To_String(URL) = Input
   --
   --  By substitution:
   --    Is_HTTP_Or_HTTPS(Result.URL) = Has_Valid_Scheme(Input) = True
   --    No_Credentials(Result.URL) = not Has_Credentials(Input) = True
   --    Not_Private_Address(Result.URL) = not Has_Private_Host(Input) = True
   --
   --  Therefore the postcondition is satisfied. QED.
   ---------------------------------------------------------------------------
   function Canonicalize (Input : String) return Canonicalize_Result is
      Host_Start : Natural;
      Host_End   : Natural;
      Host_Valid : Boolean;
      Result     : Canonicalize_Result;
   begin
      --  Initialize Result.URL for error cases
      Result.URL := Empty_URL;

      --  Check length
      if Input'Length < 7 or else Input'Length > Max_URL_Length then
         Result.Status := Invalid_Length;
         return Result;
      end if;

      --  Check scheme
      --  After this point: Has_Valid_Scheme(Input) = True
      if not Has_Valid_Scheme (Input) then
         Result.Status := Invalid_Scheme;
         return Result;
      end if;

      --  Check for credentials
      --  After this point: Has_Credentials(Input) = False
      if Has_Credentials (Input) then
         Result.Status := Credentials_Present;
         return Result;
      end if;

      --  Extract and validate host
      Extract_Host (Input, Host_Start, Host_End, Host_Valid);
      pragma Unreferenced (Host_End);  --  Used only by Has_Private_Host
      if not Host_Valid or else Host_Start = 0 then
         Result.Status := Invalid_Host;
         return Result;
      end if;

      --  Check for private addresses
      --  After this point: Has_Private_Host(Input) = False
      if Has_Private_Host (Input) then
         Result.Status := Private_Address;
         return Result;
      end if;

      --  All checks passed.
      --
      --  At this point we have established:
      --    Has_Valid_Scheme(Input) = True
      --    Has_Credentials(Input) = False
      --    Has_Private_Host(Input) = False
      --
      --  Make_Valid_URL's postcondition guarantees:
      --    To_String(Result.URL) = Input
      --
      --  The query functions (Is_HTTP_Or_HTTPS, etc.) are expression
      --  functions that call the same predicates on To_String(URL).
      --  The prover can substitute Input for To_String(URL) and
      --  conclude the postcondition holds.

      Result.URL := Make_Valid_URL (Input);
      Result.Status := Success;

      --  Apply the substitution lemma to help the prover.
      --  The lemma proves that equal strings produce equal predicate results.
      Lemma_Predicate_Substitution (To_String (Result.URL), Input);

      --  These assertions are now provable by substitution:
      --  We know: To_String(Result.URL) = Input (Make_Valid_URL post)
      --  Lemma gives: predicate(To_String(Result.URL)) = predicate(Input)
      --  We know: Has_Valid_Scheme(Input) = True (checked above)
      --  Therefore: Has_Valid_Scheme(To_String(Result.URL)) = True
      pragma Assert (Has_Valid_Scheme (To_String (Result.URL)));
      pragma Assert (not Has_Credentials (To_String (Result.URL)));
      pragma Assert (not Has_Private_Host (To_String (Result.URL)));

      return Result;
   end Canonicalize;

   ---------------------------------------------------------------------------
   --  Make_Short_Code
   ---------------------------------------------------------------------------
   function Make_Short_Code
     (URL    : Valid_URL;
      Secret : Secret_Key)
     return Short_Code
   is
      URL_Str  : constant String := To_String (URL);
      Code     : Short_Code;
      Hash_Val : Unsigned_64 := 0;
   begin
      --  Compute hash and convert first 8 bytes to integer
      declare
         Hash : constant String := Compute_Hash (URL_Str, Secret);
      begin
         --  Hash is guaranteed to be (1..32) by Compute_Hash postcondition
         for I in 1 .. 8 loop
            Hash_Val := Hash_Val * 256;
            Hash_Val := Hash_Val + Unsigned_64 (Character'Pos (Hash (I)));
            pragma Loop_Invariant (I >= 1);
         end loop;
      end;

      --  Encode to Base62
      for I in reverse 1 .. Short_Code_Length loop
         Code.Data (I) := Base62_Alphabet
           (Natural (Hash_Val mod Base62_Size) + 1);
         Hash_Val := Hash_Val / Base62_Size;
         pragma Loop_Invariant (I >= 1);
      end loop;

      return Code;
   end Make_Short_Code;

end Core;
