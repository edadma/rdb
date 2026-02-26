package io.github.edadma.petradb.server

import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import java.security.SecureRandom
import java.util.Base64

object PlatformCrypto:
  def pbkdf2(password: String, salt: Array[Byte], iterations: Int, keyLength: Int): Array[Byte] =
    val spec = new PBEKeySpec(password.toCharArray, salt, iterations, keyLength * 8)
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    factory.generateSecret(spec).getEncoded

  def randomBytes(length: Int): Array[Byte] =
    val bytes = new Array[Byte](length)
    SecureRandom().nextBytes(bytes)
    bytes

  def base64Encode(bytes: Array[Byte]): String =
    Base64.getEncoder.encodeToString(bytes)

  def base64Decode(s: String): Array[Byte] =
    Base64.getDecoder.decode(s)
