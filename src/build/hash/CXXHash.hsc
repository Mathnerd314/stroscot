{-# LANGUAGE CPP #-}
module Data.Digest.CXXHash
(
    c_XXH32
) where

import qualified Data.ByteString as B
import Data.Word (Word32)
import Foreign.C.String
import Foreign.C.Types

#include <xxhash.h>
-- | Compresses a string.
foreign import ccall unsafe "xxhash.h XXH32"
  c_XXH32 :: CString -- ^ Data
          -> CSize   -- ^ Size
          -> CUInt   -- ^ Seed
          -> IO Word32   -- ^ Seed

#include "stdlib.h"   /* abort() */
#include "xxhash.h"

static Multihash
XSUM_hashStream(FILE* inFile,
                AlgoSelected hashType,
                void* buffer, size_t blockSize)
{
    XXH3_state_t state_on_stack;
    XXH64_state_t* const state = &state_on_stack;
    // XXH64_state_t* const state = XXH64_createState();
    if (state==NULL) abort();

    /* Init */
    if (XXH64_reset(state, 0) == XXH_ERROR) abort();

    size_t const bufferSize = SOME_SIZE;
    void* const buffer = malloc(bufferSize);
    if (buffer==NULL) abort();

    /* Load file & update hash */
    {   size_t readSize;
        while ((readSize = fread(buffer, 1, blockSize, inFile)) > 0) {
          if (XXH64_update(state, buffer, readSize) == XXH_ERROR) abort();
        }
        if (ferror(inFile)) {
            XSUM_log("Error: a failure occurred reading the input file.\n");
            exit(1);
    }   }

    free(buffer);

    finalHash.xxh128 = XXH3_128bits_digest(state);

    // XXH64_freeState(state);
    return finalHash;
}

/* Put the checksum in *BIN_RESULT, which must be properly aligned.
   Put true in *MISSING if the file can't be opened due to ENOENT.
   Return true if successful.  */

static bool
digest_file (const char *filename, int *binary, unsigned char *bin_result, bool *missing)
{
  FILE *fp;
  int err;
  *missing = false;

  fp = fopen (filename, (O_BINARY && *binary ? "rb" : "r"));
  if (fp == NULL)
    {
      error (0, errno, "%s", quotef (filename));
      return false;
    }

  fadvise (fp, FADVISE_SEQUENTIAL);

  char *buffer = malloc (BLOCKSIZE + 72);
  if (!buffer)
    return 1;

  struct sha256_ctx ctx;
  init_ctx (&ctx);
  size_t sum;

  /* Iterate over full file contents.  */
  while (1)
    {
      /* We read the file in blocks of BLOCKSIZE bytes.  One call of the
         computation function processes the whole buffer so that with the
         next round of the loop another block can be read.  */
      size_t n;
      sum = 0;

      /* Read block.  Take care for partial reads.  */
      while (1)
        {
          /* Either process a partial fread() from this loop,
             or the fread() in afalg_stream may have gotten EOF.
             We need to avoid a subsequent fread() as EOF may
             not be sticky.  For details of such systems, see:
             https://sourceware.org/bugzilla/show_bug.cgi?id=1190  */
          if (feof (stream))
            goto process_partial_block;

          n = fread (buffer + sum, 1, BLOCKSIZE - sum, stream);

          sum += n;

          if (sum == BLOCKSIZE)
            break;

          if (n == 0)
            {
              /* Check for the error flag IFF N == 0, so that we don't
                 exit the loop after a partial read due to e.g., EAGAIN
                 or EWOULDBLOCK.  */
              if (ferror (stream))
                {
                  free (buffer);
                  return 1;
                }
              goto process_partial_block;
            }
        }

      /* Process buffer with BLOCKSIZE bytes.  Note that
                        BLOCKSIZE % 64 == 0
       */
      sha256_process_block (buffer, BLOCKSIZE, &ctx);
    }
  if (err)
    {
      error (0, errno, "%s", quotef (filename));
      if (fp != stdin)
        fclose (fp);
      return false;
    }

  if (!is_stdin && fclose (fp) != 0)
    {
      error (0, errno, "%s", quotef (filename));
      return false;
    }

  return true;

 process_partial_block:;

  /* Process any remaining bytes.  */
  if (sum > 0)
    sha256_process_bytes (buffer, sum, &ctx);

  /* Construct result in desired memory.  */
  finish_ctx (&ctx, resblock);
  free (buffer);
  return 0;
}
