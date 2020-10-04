package samples

import "fmt"
import "io"
import "crypto/md5"
import "crypto/sha256"

import "golang.org/x/crypto/blake2s"

// Trivial routine
func WeakHash(a int32) {
	hMd5 := md5.New()
	hSha := sha256.New()
	hBlake2s, err := blake2s.New256(nil)
	if err == nil {
		io.WriteString(hMd5, "Welcome to Go Language Secure Coding Practices")
		io.WriteString(hSha, "Welcome to Go Language Secure Coding Practices")
		io.WriteString(hBlake2s, "Welcome to Go Language Secure Coding Practices")
		fmt.Printf("MD5        : %x\n", hMd5.Sum(nil))
		fmt.Printf("SHA256     : %x\n", hSha.Sum(nil))
		fmt.Printf("Blake2s-256: %x\n", hBlake2s.Sum(nil))
	}
}
