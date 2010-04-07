/* @@FNAME@@ --
 * $Id$
 *
 * Copyright (C) @@YEAR@@ @@NAME@@
 *
 * Author: @@NAME@@ <@@ADDRESS@@>
 * Maintainer: @@NAME@@ <@@ADDRESS@@>
 * Created: @@DATE@@
 * Version: 1.0
 * Keywords:
 *
@@LICENSE@@
 */

#ifndef @@HEADER_DEFINDED@@
#define @@HEADER_DEFINDED@@


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* @@HEADER_DEFINDED@@ */

@@@@
FNAME: (file-name-nondirectory buffer-file-name)
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
DATE: (format-time-string "%d %b %Y")
LICENSE: (my-ask-license-string " * ")
HEADER_DEFINDED: (my-c-include-once-macro buffer-file-name)
@@@@
