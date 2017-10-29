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
        rsa.Encrypt(data, false)

    let private decrypt (data : byte[]) (key : RSAParameters) =
        use rsa = new RSACryptoServiceProvider()
        rsa.ImportParameters(key)
        rsa.Decrypt(data, false)

    [<Test>]
    let ``Round trip RSA key`` () =
        let csprng = Csprng.create ()
        let key,_ = runState (Rsa.createKey 4 8) csprng
        let data = "hello, world" |> Encoding.UTF8.GetBytes
        let encData = encrypt data key
        let decData = decrypt data key
        let decString = Encoding.UTF8.GetString decData
        Assert.That(decString, Is.EqualTo("hello, world"))
        ()