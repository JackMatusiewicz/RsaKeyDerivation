namespace RsaKeyDerivation.Test

module RsaTests =
    open NUnit.Framework
    open RsaKeyDerivation
    open RsaKeyDerivation.State
    open System.Security.Cryptography
    open System.Text
    open System
    open System.Linq

    let private encrypt (data : byte[]) (key : RSAParameters) =
        use rsa = new RSACryptoServiceProvider()
        rsa.ImportParameters(key)
        rsa.Encrypt(data, true)

    let private decrypt (data : byte[]) (key : RSAParameters) =
        use rsa = new RSACryptoServiceProvider()
        rsa.ImportParameters(key)
        rsa.Decrypt(data, true)

    [<Test>]
    [<Repeat(20)>]
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

    [<Test>]
    [<Repeat(10)>]
    let ``Using the same csprng gives you exactly the same Rsa key``() =
        let keysAreIdentical (a : RSAParameters) (b : RSAParameters) =
            Assert.That(a.D.SequenceEqual(b.D), Is.True)
            Assert.That(a.DP.SequenceEqual(b.DP), Is.True)
            Assert.That(a.DQ.SequenceEqual(b.DQ), Is.True)
            Assert.That(a.Exponent.SequenceEqual(b.Exponent), Is.True)
            Assert.That(a.InverseQ.SequenceEqual(b.InverseQ), Is.True)
            Assert.That(a.Modulus.SequenceEqual(b.Modulus), Is.True)
            Assert.That(a.P.SequenceEqual(b.P), Is.True)
            Assert.That(a.Q.SequenceEqual(b.Q), Is.True)
        let csprng = Csprng.create ()
        let key,_ = runState (Rsa.createKey 1024 8) csprng
        let key2,_ = runState (Rsa.createKey 1024 8) csprng
        keysAreIdentical key key2
