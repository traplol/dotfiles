#!/usr/bin/env python3

import dbus
import argparse
import inspect
from datetime import timedelta

arg_parser = argparse.ArgumentParser("spotify info")
arg_parser.add_argument(
    "--format",
    default='%(title)s by %(artist)s',
    type=str,
    help='the output format, available params are "title", "album", "artist", "length" (default:"%%(title)s by %%(artist)s")'
)

arg_parser.add_argument(
    '--color',
    default="#ffffff",
    type=str,
    help='the html hex color string to output (default:#ffffff)'
)
args = arg_parser.parse_args()


def main():
    spotify = dbus.SessionBus().get_object('org.mpris.MediaPlayer2.spotify', '/org/mpris/MediaPlayer2')
    properties = dbus.Interface(spotify, 'org.freedesktop.DBus.Properties')
    metadata = properties.Get('org.mpris.MediaPlayer2.Player', 'Metadata')

    title = metadata['xesam:title']
    artist = metadata['xesam:artist'][0]
    album = metadata['xesam:album']
    length = metadata['mpris:length']
    fmt_params = {
        "title" : title,
        "album" : album,
        "artist" : artist,
        "length" : timedelta(seconds=int(length/1000000)), # length is in microseconds
    }
    out = args.format % fmt_params

    print(out)
    print(out)
    print(args.color)

if __name__ == "__main__":
    try:
        main()
    except:
        pass
