namespace RsaKeyDerivation.Test

module RsaTests =
    open NUnit.Framework
    open RsaKeyDerivation
    open RsaKeyDerivation.State
    open System.Security.Cryptography
    open System.Text
    open System

    let private encrypt (data : byte[]) (key : RSAParameters) =
        use rsa = new RSACryptoServiceProvider()
        rsa.ImportParameters(key)
        rsa.Encrypt(data, true)

    let private decrypt (data : byte[]) (key : RSAParameters) =
        use rsa = new RSACryptoServiceProvider()
        rsa.ImportParameters(key)
        rsa.Decrypt(data, true)

    [<Test>]
    [<Repeat(2)>]
    let ``Round trip RSA key`` () =
        let csprng = Csprng.create ()
        let byteConverter = new UnicodeEncoding();
        let key,_ = runState (Rsa.createKey 1024 8) csprng
        let data = "hello, world" |> byteConverter.GetBytes
        let encData = encrypt data key
        let decData = decrypt encData key
        let decString = byteConverter.GetString decData
        Assert.That(decString, Is.EqualTo("hello, world"))
        ()