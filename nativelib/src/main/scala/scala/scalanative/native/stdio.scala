package scala.scalanative
package native

@extern
object stdio {

  // Byte access

  def fopen(filename: CString, mode: CString): Ptr[Byte]  = extern
  def fclose(stream: Ptr[Byte]): CInt                     = extern
  def fflush(stream: Ptr[Byte]): CInt                     = extern
  def setbuf(stream: Ptr[Byte], buffer: Ptr[CChar]): Unit = extern
  def setvbuf(stream: Ptr[Byte],
              buffer: Ptr[CChar],
              mode: CInt,
              size: CSize): CInt =
    extern
  def fwide(stream: Ptr[Byte], mode: CInt): CInt = extern

  // Direct input/output

  def fread(buffer: Ptr[_],
            size: CSize,
            count: CSize,
            stream: Ptr[Byte]): CSize =
    extern
  def fwrite(buffer: Ptr[_],
             size: CSize,
             count: CSize,
             stream: Ptr[Byte]): CSize =
    extern

  // Unformatted input/output

  def fgetc(stream: Ptr[Byte]): CInt                               = extern
  def getc(stream: Ptr[Byte]): CInt                                = extern
  def fgets(str: CString, count: CInt, stream: Ptr[Byte]): CString = extern
  def fputc(ch: CInt, stream: Ptr[Byte]): CInt                     = extern
  def putc(ch: CInt, stream: Ptr[Byte]): CInt                      = extern
  def fputs(str: CString, stream: Ptr[Byte]): CInt                 = extern
  def getchar(): CInt                                              = extern
  def gets(str: CString): CString                                  = extern
  def putchar(ch: CInt): CInt                                      = extern
  def puts(str: CString): CInt                                     = extern
  def ungetc(ch: CInt, stream: Ptr[Byte]): CInt                    = extern

  // Formatted input/output

  def scanf(format: CString, args: Vararg*): CInt                      = extern
  def fscanf(stream: Ptr[Byte], format: CString, args: Vararg*): CInt  = extern
  def sscanf(buffer: CString, format: CString, args: Vararg*): CInt    = extern
  def printf(format: CString, args: Vararg*): CInt                     = extern
  def fprintf(stream: Ptr[Byte], format: CString, args: Vararg*): CInt = extern
  def sprintf(buffer: CString, format: CString, args: Vararg*): CInt   = extern
  def snprintf(buffer: CString,
               bufsz: CInt,
               format: CString,
               args: Vararg*): CInt =
    extern

  // Byte positioning

  def ftell(stream: Ptr[Byte]): CLong                             = extern
  def fgetpos(stream: Ptr[Byte], pos: Ptr[Byte]): CInt            = extern
  def fseek(stream: Ptr[Byte], offset: CLong, origin: CInt): CInt = extern
  def fsetpos(stream: Ptr[Byte], pos: Ptr[Byte]): CInt            = extern
  def rewind(stream: Ptr[Byte]): Unit                             = extern

  // Error handling

  def clearerr(stream: Ptr[Byte]): Unit = extern
  def feof(stream: Ptr[Byte]): CInt     = extern
  def ferror(stream: Ptr[Byte]): CInt   = extern
  def perror(str: CString): Unit        = extern

  // Operations on files

  def remove(fname: CString): CInt                             = extern
  def rename(oldByteName: CString, newByteName: CString): CInt = extern
  def tmpfile(): Ptr[Byte]                                     = extern
  def tmpnam(fileName: CString): CString                       = extern

  // Macros

  @name("scalanative_libc_stdin")
  def stdin: Ptr[Byte] = extern
  @name("scalanative_libc_stdout")
  def stdout: Ptr[Byte] = extern
  @name("scalanative_libc_stderr")
  def stderr: Ptr[Byte] = extern
  @name("scalanative_libc_eof")
  def EOF: CInt = extern
  @name("scalanative_libc_fopen_max")
  def FOPEN_MAX: CUnsignedInt = extern
  @name("scalanative_libc_filename_max")
  def ByteNAME_MAX: CUnsignedInt = extern
  @name("scalanative_libc_bufsiz")
  def BUFSIZ: CUnsignedInt = extern
  @name("scalanative_libc_iofbf")
  def _IOFBF: CInt = extern
  @name("scalanative_libc_iolbf")
  def _IOLBF: CInt = extern
  @name("scalanative_libc_ionbf")
  def _IONBF: CInt = extern
  @name("scalanative_libc_seek_set")
  def SEEK_SET: CInt = extern
  @name("scalanative_libc_seek_cur")
  def SEEK_CUR: CInt = extern
  @name("scalanative_libc_seek_end")
  def SEEK_END: CInt = extern
  @name("scalanative_libc_tmp_max")
  def TMP_MAX: CUnsignedInt = extern
  @name("scalanative_libc_l_tmpnam")
  def L_tmpnam: CUnsignedInt = extern

  // TODO:
  // @struct class FILE private ()
  // @struct class fpos_t private ()
}
