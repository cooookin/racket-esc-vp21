#lang scribble/manual

@require[(for-label esc/vp21)
         (for-label typed/racket/base)
         (for-label typed/racket/class)]

@title{Epson Projector Control}
@author+email["Jan Dvořák" "mordae@anilinux.org"]


@defmodule[esc/vp21]

This library allows for control of Epson Projectors via TCP/IP.

@section{Interface}

@defidform[Projector%]{
  Type of the @racket[projector%] class.
}

@defidform[Power-Status]{
  Type representing power status the projector can found itself in.
  Equivalent to @racket[(U 'offline 'online 'warmup 'cooldown 'standby
  'abnormal 'av-standby 'unknown)].
}

@defidform[Projector-Aspect]{
  Projector picture aspect mode. Equivalent to @racket[(U 'normal '4:3 '16:9
  'auto 'full 'zoom 'native)].
}

@defclass[projector% object% ()]{
  Remotely controlled Epson projector instance.

  @defconstructor[((host String))]{
    Connect to projector running on specified address or a host name
    passed as the @racket[host] field.
  }

  @defmethod[(command (fmt String) (arg String) ...) String]{
    Execute raw @tt{ESC/VP21} command string composed in the same style as
    with the @racket[format] function. Returns reply.
  }

  @defmethod[(get-power-status) Power-Status]{
    Inquire about projector power status.
  }

  @defmethod[(online!) Void]{
    Turn the projector on.

    In takes a while for the projector to warm up. It is possible to
    monitor situation by polling the @racket[get-power-status] method.
  }

  @defmethod[(offline!) Void]{
    Shut the projector down.

    Like startup, cooling down the lamp takes some time. It is possible
    to monitor situation by polling the @racket[get-power-status] method.
  }

  @defmethod[(set-mute! (mute? Boolean)) Void]{
    Enable or disable the video mute function.
    Basically blanks or un-blanks the projection.
    Muting the picture unfreezes it.
  }

  @defmethod[(set-freeze! (freeze? Boolean)) Void]{
    Freeze or unfreeze the current picture.
    Freezing the picture unmutes it.
  }

  @defmethod[(set-aspect! (aspect Projector-Aspect)) Void]{
    Change projector picture aspect mode.
  }

  @defmethod[(get-mute?) Boolean]{
    Get video mute status.
  }

  @defmethod[(get-freeze?) Boolean]{
    Get picture freeze status.
  }

  @defmethod[(get-aspect) Projector-Aspect]{
    Get current picture aspect mode.
  }

  @defmethod[(send-key (key Byte)) Void]{
    Send a keycode press. Refer to manual for your projector revision to get
    list of valid keycodes and their meaning.
  }
}

@; vim:set ft=scribble sw=2 ts=2 et:
