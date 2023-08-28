# Haskell-Bencode
Bencode Encoder and Decoder made using haskell.

Bencode (pronounced like Bee-encode) is the encoding used by the peer-to-peer file sharing system [BitTorrent](https://en.wikipedia.org/wiki/BitTorrent_(protocol)) for storing and transmitting loosely structured data.[1]
It supports four different types of values:
byte strings,
integers,
lists, and
[dictionaries associative arrays](https://en.wikipedia.org/wiki/Associative_array).
Bencoding is most commonly used in [torrent](https://en.wikipedia.org/wiki/Torrent_file) files, and as such is part of the BitTorrent specification. These [metadata](https://en.wikipedia.org/wiki/Metadata) files are simply bencoded dictionaries.

This project decoded and encodes from and to Bencode with support for the 4 datatypes.
Format : 
Strings:
Decoded: "example"
Encoded: "7:example"
Integers:
Decoded: 42
Encoded: "i42e"
Lists:
Decoded: ["foo", 123]
Encoded: "l3:fooi123ee"
Dictionaries:
Decoded: {"bar": "baz", "foo": 42}
Encoded: "d3:bar3:baze3:fooi42ee"
