-- Core implementation with SPARK proofs
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Core is

   -- Base62 alphabet for encoding
   Base62_Alphabet : constant String := 
     "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

   -- Private IP ranges (simplified checks)
   function Is_Private_IP (Host : String) return Boolean is
   begin
      -- Check for common private prefixes
      if Host'Length >= 3 then
         if Host (Host'First .. Host'First + 2) = "10." or else
            Host (Host'First .. Host'First + 2) = "127" then
            return True;
         end if;
      end if;
      
      if Host'Length >= 7 then
         if Host (Host'First .. Host'First + 6) = "192.168" or else
            Host (Host'First .. Host'First + 6) = "172.16." or else
            Host (Host'First .. Host'First + 6) = "172.17." or else
            Host (Host'First .. Host'First + 6) = "172.18." or else
            Host (Host'First .. Host'First + 6) = "172.19." or else
            Host (Host'First .. Host'First + 6) = "172.20." or else
            Host (Host'First .. Host'First + 6) = "172.21." or else
            Host (Host'First .. Host'First + 6) = "172.22." or else
            Host (Host'First .. Host'First + 6) = "172.23." or else
            Host (Host'First .. Host'First + 6) = "172.24." or else
            Host (Host'First .. Host'First + 6) = "172.25." or else
            Host (Host'First .. Host'First + 6) = "172.26." or else
            Host (Host'First .. Host'First + 6) = "172.27." or else
            Host (Host'First .. Host'First + 6) = "172.28." or else
            Host (Host'First .. Host'First + 6) = "172.29." or else
            Host (Host'First .. Host'First + 6) = "172.30." or else
            Host (Host'First .. Host'First + 6) = "172.31." then
            return True;
         end if;
      end if;
      
      return False;
   end Is_Private_IP;

   -- Check if URL has valid HTTP/HTTPS scheme
   function Has_Valid_Scheme (Input : String) return Boolean is
   begin
      if Input'Length < 7 then  -- Minimum "http://"
         return False;
      end if;
      
      if Input'Length >= 8 and then
         Input (Input'First .. Input'First + 7) = "https://" then
         return True;
      elsif Input'Length >= 7 and then
         Input (Input'First .. Input'First + 6) = "http://" then
         return True;
      else
         return False;
      end if;
   end Has_Valid_Scheme;

   -- Check for credentials (user:pass@)
   function Has_Credentials (Input : String) return Boolean is
   begin
      for I in Input'Range loop
         if Input (I) = '@' then
            -- Check if there's a colon before @
            for J in Input'First .. I - 1 loop
               if Input (J) = ':' then
                  return True;
               end if;
            end loop;
         end if;
      end loop;
      return False;
   end Has_Credentials;

   -- Extract host portion from URL
   procedure Extract_Host
     (Input : String;
      Host_Start : out Natural;
      Host_End : out Natural;
      Valid : out Boolean)
   is
      Idx : Natural;
   begin
      Host_Start := 0;
      Host_End := 0;
      Valid := False;
      
      -- Skip scheme
      if Input'Length >= 8 and then
         Input (Input'First .. Input'First + 7) = "https://" then
         Idx := Input'First + 8;
      elsif Input'Length >= 7 and then
         Input (Input'First .. Input'First + 6) = "http://" then
         Idx := Input'First + 7;
      else
         return;
      end if;
      
      Host_Start := Idx;
      
      -- Find end of host (first '/', '?', '#', or end of string)
      while Idx <= Input'Last loop
         if Input (Idx) = '/' or else
            Input (Idx) = '?' or else
            Input (Idx) = '#' then
            Host_End := Idx - 1;
            Valid := True;
            return;
         end if;
         Idx := Idx + 1;
      end loop;
      
      -- Host extends to end of string
      Host_End := Input'Last;
      Valid := True;
   end Extract_Host;

   function Canonicalize (Input : String) return Canonicalize_Result
   is
      Host_Start : Natural;
      Host_End : Natural;
      Host_Valid : Boolean;
      Result : Canonicalize_Result;
   begin
      -- Check length
      if Input'Length = 0 or else Input'Length > Max_URL_Length then
         Result.Status := Invalid_Length;
         return Result;
      end if;
      
      -- Check scheme
      if not Has_Valid_Scheme (Input) then
         Result.Status := Invalid_Scheme;
         return Result;
      end if;
      
      -- Check for credentials
      if Has_Credentials (Input) then
         Result.Status := Credentials_Present;
         return Result;
      end if;
      
      -- Extract and validate host
      Extract_Host (Input, Host_Start, Host_End, Host_Valid);
      if not Host_Valid or else Host_Start = 0 then
         Result.Status := Invalid_Host;
         return Result;
      end if;
      
      -- Check for private addresses
      declare
         Host : constant String := Input (Host_Start .. Host_End);
      begin
         if Is_Private_IP (Host) then
            Result.Status := Private_Address;
            return Result;
         end if;
      end;
      
      -- URL is valid - copy to output
      Result.URL.Data (1 .. Input'Length) := Input;
      Result.URL.Len := Input'Length;
      Result.Status := Success;
      
      return Result;
   end Canonicalize;

   -- Simple HMAC-SHA256 placeholder (simplified for now)
   -- In production, this would use a proper cryptographic library
   function Compute_HMAC
     (Message : String;
      Key : Secret_Key)
     return String
   is
      Result : String (1 .. 32);
      Hash_Val : Unsigned_32;
   begin
      -- Simplified hash computation
      Hash_Val := 0;
      for I in Message'Range loop
         Hash_Val := Hash_Val xor Unsigned_32 (Character'Pos (Message (I)));
         Hash_Val := Hash_Val * 31;
      end loop;
      
      for I in Key'Range loop
         Hash_Val := Hash_Val xor Unsigned_32 (Character'Pos (Key (I)));
         Hash_Val := Hash_Val * 17;
      end loop;
      
      -- Convert to hex string
      for I in Result'Range loop
         Result (I) := Character'Val (48 + Integer (Hash_Val mod 10));
         Hash_Val := Hash_Val / 10;
      end loop;
      
      return Result;
   end Compute_HMAC;

   function Make_Short_Code
     (URL    : Valid_URL;
      Secret : Secret_Key)
     return Short_Code
   is
      URL_Str : constant String := To_String (URL);
      Hash : constant String := Compute_HMAC (URL_Str, Secret);
      Code : Short_Code;
      Hash_Val : Unsigned_64 := 0;
   begin
      -- Convert hash bytes to integer
      for I in Hash'First .. Hash'First + 7 loop
         Hash_Val := Hash_Val * 256;
         Hash_Val := Hash_Val + Unsigned_64 (Character'Pos (Hash (I)));
      end loop;
      
      -- Encode to Base62
      for I in reverse Code.Data'Range loop
         Code.Data (I) := Base62_Alphabet 
           (Natural (Hash_Val mod Base62_Size) + 1);
         Hash_Val := Hash_Val / Base62_Size;
      end loop;
      
      return Code;
   end Make_Short_Code;

   -- Query functions
   function Is_HTTP_Or_HTTPS (URL : Valid_URL) return Boolean is
      URL_Str : constant String := To_String (URL);
   begin
      return Has_Valid_Scheme (URL_Str);
   end Is_HTTP_Or_HTTPS;

   function Not_Private_Address (URL : Valid_URL) return Boolean is
      URL_Str : constant String := To_String (URL);
      Host_Start, Host_End : Natural;
      Host_Valid : Boolean;
   begin
      Extract_Host (URL_Str, Host_Start, Host_End, Host_Valid);
      if not Host_Valid then
         return False;
      end if;
      
      return not Is_Private_IP (URL_Str (Host_Start .. Host_End));
   end Not_Private_Address;

   function No_Credentials (URL : Valid_URL) return Boolean is
      URL_Str : constant String := To_String (URL);
   begin
      return not Has_Credentials (URL_Str);
   end No_Credentials;

   function Length (Code : Short_Code) return Natural is
   begin
      return Code.Data'Length;
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
