/*===========================================================================*
 * mpeg.h								     *
 *									     *
 *	no comment							     *
 *									     *
 *===========================================================================*/

/*
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*  
 *  $Header: /home/daf/mpeg/mpgwrite/RCS/mpeg.h,v 1.1 1994/01/07 17:05:21 daf Exp $
 *  $Log: mpeg.h,v $
 * Revision 1.1  1994/01/07  17:05:21  daf
 * Initial revision
 *
 * Revision 1.4  1993/07/22  22:24:23  keving
 * nothing
 *
 * Revision 1.3  1993/07/09  00:17:23  keving
 * nothing
 *
 * Revision 1.2  1993/06/03  21:08:53  keving
 * nothing
 *
 * Revision 1.1  1993/02/17  23:18:20  dwallach
 * Initial revision
 *
 */


/*==============*
 * HEADER FILES *
 *==============*/

#include "ansi.h"
#include "mtypes.h"
#include "frame.h"


/*===============================*
 * EXTERNAL PROCEDURE prototypes *
 *===============================*/

void SetFramePattern _ANSI_ARGS_((char *pattern));
int32 GenMPEGStream _ANSI_ARGS_((int whichGOP, int frameStart, int frameEnd,
				int numFrames, FILE **ofp,
				char *outputFileName));
extern void	PrintStartStats _ANSI_ARGS_((int firstFrame, int lastFrame));
extern void	IncrementTCTime _ANSI_ARGS_((void));
void	SetReferenceFrameType _ANSI_ARGS_((char *type));
boolean	NonLocalRefFrame _ANSI_ARGS_((int id));
extern void ReadDecodedRefFrame _ANSI_ARGS_((MpegFrame *frame,
					     int frameNumber));
extern void	WriteDecodedFrame _ANSI_ARGS_((MpegFrame *frame));


/*==================*
 * GLOBAL VARIABLES *
 *==================*/

extern MpegFrame *frameMemory[3];
extern int32	  tc_hrs, tc_min, tc_sec, tc_pict;
extern int	  totalFramesSent;
extern int	  gopSize;
extern char	 *framePattern;
extern int	  framePatternLen;