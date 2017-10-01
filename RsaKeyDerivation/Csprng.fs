namespace RsaKeyDerivation

open System.Security.Cryptography

type Csprng = {
    Counter : bigint
    Key : AesManaged
}

module Csprng =
    let create () =
        let aes = new AesManaged()
        aes.Mode <- CipherMode.ECB
        {Counter = bigint 0; Key = aes}

