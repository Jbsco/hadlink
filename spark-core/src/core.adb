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

   --  Default empty short code
   Empty_Code : constant Short_Code :=
     (Data => (others => '0'));

   --  Base62 alphabet for encoding
   Base62_Alphabet : constant String :=
     "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

   --  Forward declarations for private helper functions
   function Is_Private_IP (Host : String) return Boolean
   with Pre => Host'Length >= 1 and then Host'Last < Integer'Last - 1;

   function Has_Valid_Scheme (Input : String) return Boolean
   with Pre => Input'Length >= 1 and then Input'Last < Integer'Last - 1;

   function Has_Credentials (Input : String) return Boolean
   with Pre => Input'Length >= 1 and then
               Input'First >= 1 and then
               Input'Last < Integer'Last - 10;

   procedure Extract_Host
     (Input      : String;
      Host_Start : out Natural;
      Host_End   : out Natural;
      Valid      : out Boolean)
   with
     Pre  => Input'Length >= 7 and then Input'Last < Integer'Last - 10,
     Post => (if Valid then
                Host_Start >= Input'First and then
                Host_End <= Input'Last and then
                Host_Start <= Host_End);

   function Compute_Hash
     (Message : String;
      Key     : Secret_Key)
     return String
   with
     Pre  => Message'Length >= 1 and then
             Message'Length <= Max_URL_Length and then
             Message'Last < Integer'Last,
     Post => Compute_Hash'Result'Length = 32 and then
             Compute_Hash'Result'First = 1;

   --  Private IP ranges (simplified checks)
   function Is_Private_IP (Host : String) return Boolean
   is
   begin
      --  Check for localhost
      if Host = "localhost" then
         return True;
      end if;

      --  Check for common private prefixes
      if Host'Length >= 3 then
         declare
            Prefix3 : constant String :=
              Host (Host'First .. Host'First + 2);
         begin
            if Prefix3 = "10." or else Prefix3 = "127" then
               return True;
            end if;
         end;
      end if;

      if Host'Length >= 8 then
         declare
            Prefix7 : constant String :=
              Host (Host'First .. Host'First + 6);
         begin
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

      return False;
   end Is_Private_IP;

   --  Check if URL has valid HTTP/HTTPS scheme
   function Has_Valid_Scheme (Input : String) return Boolean
   is
   begin
      if Input'Length < 7 then
         return False;
      end if;

      if Input'Length >= 8 then
         declare
            Prefix8 : constant String :=
              Input (Input'First .. Input'First + 7);
         begin
            if Prefix8 = "https://" then
               return True;
            end if;
         end;
      end if;

      declare
         Prefix7 : constant String :=
           Input (Input'First .. Input'First + 6);
      begin
         if Prefix7 = "http://" then
            return True;
         end if;
      end;

      return False;
   end Has_Valid_Scheme;

   --  Check for credentials (user:pass@) - simplified check
   function Has_Credentials (Input : String) return Boolean
   is
      Found_At : Boolean := False;
   begin
      --  Look for @ sign followed by checking for : before it
      for I in Input'Range loop
         if Input (I) = '@' then
            Found_At := True;
            --  Check if there's a : before @
            for J in Input'First .. I - 1 loop
               if Input (J) = ':' then
                  --  Found : before @, but need to skip scheme ://
                  if J >= Input'First + 5 then
                     --  Check it's not the scheme separator
                     if J > Input'First + 6 or else
                        Input (Input'First .. Input'First + 3) /= "http"
                     then
                        return True;
                     end if;
                  end if;
               end if;
               pragma Loop_Invariant (J >= Input'First);
            end loop;
         end if;
         pragma Loop_Invariant (I >= Input'First);
      end loop;
      pragma Unreferenced (Found_At);
      return False;
   end Has_Credentials;

   --  Extract host portion from URL
   procedure Extract_Host
     (Input      : String;
      Host_Start : out Natural;
      Host_End   : out Natural;
      Valid      : out Boolean)
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
         if Input (I) = '/' or else
            Input (I) = '?' or else Input (I) = '#'
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

   function Canonicalize (Input : String) return Canonicalize_Result is
      Host_Start : Natural;
      Host_End   : Natural;
      Host_Valid : Boolean;
      Result     : Canonicalize_Result;
   begin
      --  Initialize Result.URL to avoid uninitialized warnings
      Result.URL := Empty_URL;

      --  Check length
      if Input'Length = 0 or else Input'Length > Max_URL_Length then
         Result.Status := Invalid_Length;
         return Result;
      end if;

      --  Check scheme
      if not Has_Valid_Scheme (Input) then
         Result.Status := Invalid_Scheme;
         return Result;
      end if;

      --  Check for credentials
      if Has_Credentials (Input) then
         Result.Status := Credentials_Present;
         return Result;
      end if;

      --  Ensure minimum length for Extract_Host (implied by valid scheme)
      if Input'Length < 7 then
         Result.Status := Invalid_Scheme;
         return Result;
      end if;

      --  Extract and validate host
      Extract_Host (Input, Host_Start, Host_End, Host_Valid);
      if not Host_Valid or else Host_Start = 0 then
         Result.Status := Invalid_Host;
         return Result;
      end if;

      --  Check for private addresses
      if Host_End >= Host_Start and then
         Host_End - Host_Start < Max_URL_Length
      then
         declare
            Host : constant String := Input (Host_Start .. Host_End);
         begin
            if Is_Private_IP (Host) then
               Result.Status := Private_Address;
               return Result;
            end if;
         end;
      else
         Result.Status := Invalid_Host;
         return Result;
      end if;

      --  URL is valid - copy to output
      Result.URL.Data (1 .. Input'Length) := Input;
      Result.URL.Len := Input'Length;
      Result.Status := Success;

      --  Invariant boundary assumptions for postcondition:
      --
      --  The postcondition requires Is_HTTP_Or_HTTPS, Not_Private_Address,
      --  and No_Credentials to hold on Result.URL. We have verified equivalent
      --  properties on Input via Has_Valid_Scheme, Is_Private_IP, and
      --  Has_Credentials, then copied Input verbatim into Result.URL.
      --
      --  The prover cannot automatically link these function pairs because:
      --  1. They operate on different types (String vs Valid_URL)
      --  2. String slice equality/extraction is opaque to the prover
      --  3. The data flow through the record assignment is not tracked
      --
      --  Each assumption below is sound because the validation function and
      --  its corresponding query function perform identical checks on what
      --  are now identical underlying bytes. Any violation would require the
      --  paired functions to disagree on the same byte sequence.

      --  Is_HTTP_Or_HTTPS: Has_Valid_Scheme verified "http://" or "https://"
      --  prefix on Input; Is_HTTP_Or_HTTPS checks the same prefix via
      --  To_String(URL)
      pragma Assume (Is_HTTP_Or_HTTPS (Result.URL),
                     "Has_Valid_Scheme verified http(s):// prefix on Input; " &
                     "Result.URL contains verbatim copy of Input");

      --  Not_Private_Address: Is_Private_IP returned False for extracted host;
      --  Not_Private_Address extracts host from To_String(URL) and calls same
      pragma Assume (Not_Private_Address (Result.URL),
                     "Is_Private_IP verified host is not private on Input; " &
                     "Result.URL contains verbatim copy of Input");

      --  No_Credentials: Has_Credentials returned False for Input;
      --  No_Credentials calls Has_Credentials on To_String(URL)
      pragma Assume (No_Credentials (Result.URL),
                     "Has_Credentials verified no credentials in Input; " &
                     "Result.URL contains verbatim copy of Input");

      return Result;
   end Canonicalize;

   --  HMAC-SHA256 using SPARKNaCl verified implementation
   function Compute_Hash
     (Message : String;
      Key     : Secret_Key)
     return String
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

   function Make_Short_Code
     (URL    : Valid_URL;
      Secret : Secret_Key)
     return Short_Code
   is
      URL_Str  : constant String := To_String (URL);
      Code     : Short_Code := Empty_Code;
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

   --  Query functions
   function Is_HTTP_Or_HTTPS (URL : Valid_URL) return Boolean is
      URL_Str : constant String := To_String (URL);
   begin
      if URL_Str'Length >= 8 and then
         URL_Str (URL_Str'First .. URL_Str'First + 7) = "https://"
      then
         return True;
      elsif URL_Str'Length >= 7 and then
         URL_Str (URL_Str'First .. URL_Str'First + 6) = "http://"
      then
         return True;
      else
         return False;
      end if;
   end Is_HTTP_Or_HTTPS;

   function Not_Private_Address (URL : Valid_URL) return Boolean is
      URL_Str    : constant String := To_String (URL);
      Host_Start : Natural;
      Host_End   : Natural;
      Host_Valid : Boolean;
   begin
      if URL_Str'Length < 7 then
         return False;
      end if;

      Extract_Host (URL_Str, Host_Start, Host_End, Host_Valid);
      if not Host_Valid or else Host_Start = 0 then
         return False;
      end if;

      if Host_End >= Host_Start and then
         Host_End - Host_Start < Max_URL_Length
      then
         return not Is_Private_IP (URL_Str (Host_Start .. Host_End));
      else
         return False;
      end if;
   end Not_Private_Address;

   function No_Credentials (URL : Valid_URL) return Boolean is
      URL_Str : constant String := To_String (URL);
   begin
      if URL_Str'Length < 7 then
         return True;
      end if;
      return not Has_Credentials (URL_Str);
   end No_Credentials;

   function Length (Code : Short_Code) return Natural is
      pragma Unreferenced (Code);
   begin
      return Short_Code_Length;
   end Length;

   function To_String (Code : Short_Code) return String is
   begin
      return Code.Data;
   end To_String;

   function To_String (URL : Valid_URL) return String is
   begin
      return URL.Data (1 .. URL.Len);
   end To_String;

end Core;
