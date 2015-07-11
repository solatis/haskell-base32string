{ mkDerivation, aeson, base, binary, bytestring, hspec, stdenv
, text
}:
mkDerivation {
  pname = "base32string";
  version = "0.9.1";
  src = ./.;
  buildDepends = [ aeson base binary bytestring text ];
  testDepends = [ base binary bytestring hspec text ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Fast and safe representation of a Base-32 string";
  license = stdenv.lib.licenses.mit;
}
