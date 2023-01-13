# UPNP Search Grammar

An ANTLR4 grammar for [UPNP search syntax](http://upnp.org/specs/av/UPnP-av-ContentDirectory-v2-Service.pdf).

* Find all music tracks that contain FindThis in the title.

```
upnp:class derivedfrom "object.item.audioItem.musicTrack" and (dc:title contains "FindThis")
```


* Find all music tracks in the FindThis album.

```
upnp:class derivedfrom "object.item.audioItem.musicTrack" and (upnp:album contains "FindThis")
```

* Find all music tracks with FindThis as the artist, and FindThat as the album.

```
upnp:class derivedfrom "object.item.audioItem.musicTrack" and (upnp:artist contains "FindThis")
```

* Find all music albums with FindThis as the genre.

```
upnp:class derivedfrom "object.item.audioItem.musicTrack" and (upnp:genre contains "FindThis")
```

## Exact Searches

* Find a music track with "FindThis" as the title.

```
upnp:class derivedfrom "object.item.audioItem.musicTrack" and (dc:title = "FindThis")
```
