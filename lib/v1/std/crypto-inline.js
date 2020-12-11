/*---------------------------------------------------------------------------
-- Copyright 2012-2016 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var md5algo;

if ($std_core.host() === "node") 
{
  var crypto = require('crypto');
  md5algo = function(s) {
    return crypto.createHash('md5').update(s,"utf8").digest('hex');
  };
}
else 
{
  md5algo = (function()
  {
    function stringToUTF8( s ) {
      var utf8 = []
      for( var i = 0; i < s.length; i++) {
        var c = s.charCodeAt(i);
        if (c <= 0x7F) {
          utf8.push(c);
        }
        else if (c <= 0x07FF) {
          utf8.push( 0xC0 | (c>>>6) );
          utf8.push( 0x80 | (c & 0x3F) );      
        } 
        else if (c <= 0xFFFF) {
          utf8.push( 0xE0 | (c>>>12) );
          utf8.push( 0x80 | ((c>>>6) & 0x3F) );
          utf8.push( 0x80 | (c & 0x3F) );      
        } 
        else if (c <= 0x1FFFFF) {
          utf8.push( 0xF0 | (c>>>18) );
          utf8.push( 0x80 | ((c>>>12) & 0x3F) );
          utf8.push( 0x80 | ((c>>>6) & 0x3F) );
          utf8.push( 0x80 | (c & 0x3F) );      
        } 
        else if (c <= 0x3FFFFFF) {
          utf8.push( 0xF8 | (c>>>24) );
          utf8.push( 0x80 | ((c>>>18) & 0x3F) );
          utf8.push( 0x80 | ((c>>>12) & 0x3F) );
          utf8.push( 0x80 | ((c>>>6) & 0x3F) );
          utf8.push( 0x80 | (c & 0x3F) );      
        } 
        else {
          utf8.push( 0xFC | (c>>>30) & 0x01 );
          utf8.push( 0x80 | ((c>>>24) & 0x3F) );
          utf8.push( 0x80 | ((c>>>18) & 0x3F) );
          utf8.push( 0x80 | ((c>>>12) & 0x3F) );
          utf8.push( 0x80 | ((c>>>6) & 0x3F) );
          utf8.push( 0x80 | (c & 0x3F) );      
        } 
      }
      return utf8;
    }
  
    // bytes to big-endian words
    function bytesToWords( bs ) {
      for (var ws = [], i = 0, b = 0; i < bs.length; i++, b += 8)
        ws[b >>> 5] |= bs[i] << (24 - b % 32);
      return ws;
    }

    // big-endian words to a byte array
    function wordsToBytes(ws) {
      for (var bs = [], b = 0; b < ws.length * 32; b += 8)
        bs.push((ws[b >>> 5] >>> (24 - b % 32)) & 0xFF);
      return bs;
    }

    // bytes to a hex string
    function bytesToHex(bs) {
      for (var hex = [], i = 0; i < bs.length; i++) {
        hex.push((bs[i] >>> 4).toString(16));
        hex.push((bs[i] & 0xF).toString(16));
      }
      return hex.join('');
    }

    // swap endianness of a word array
    function wordsSwapEndian( ws ) {
      for (var i = 0; i < ws.length; i++) {
        ws[i] = ((ws[i] <<  8) | (ws[i] >>> 24)) & 0x00FF00FF |
                ((ws[i] << 24) | (ws[i] >>>  8)) & 0xFF00FF00;
      }
      return ws;
    }

    // helpers
    function digest1(a, b, c, d, x, s, t) {
      var n = a + (b & c | ~b & d) + (x >>> 0) + t;
      return ((n << s) | (n >>> (32 - s))) + b;
    };
    function digest2(a, b, c, d, x, s, t) {
      var n = a + (b & d | c & ~d) + (x >>> 0) + t;
      return ((n << s) | (n >>> (32 - s))) + b;
    };
    function digest3(a, b, c, d, x, s, t) {
      var n = a + (b ^ c ^ d) + (x >>> 0) + t;
      return ((n << s) | (n >>> (32 - s))) + b;
    };
    function digest4(a, b, c, d, x, s, t) {
      var n = a + (c ^ (b | ~d)) + (x >>> 0) + t;
      return ((n << s) | (n >>> (32 - s))) + b;
    };
    
    var md5digest = function(input) 
    {    
      var msg = stringToUTF8(input);
      var m = wordsSwapEndian(bytesToWords(msg)),
          l = msg.length * 8,
          a =  1732584193,
          b = -271733879,
          c = -1732584194,
          d =  271733878;

      m[l >>> 5] |= 0x80 << (l % 32);
      m[(((l + 64) >>> 9) << 4) + 14] = l;

      for (var i = 0; i < m.length; i += 16) 
      {
        var a0 = a, b0 = b, c0 = c, d0 = d;

        a = digest1(a, b, c, d, m[i+ 0],  7, -680876936);
        d = digest1(d, a, b, c, m[i+ 1], 12, -389564586);
        c = digest1(c, d, a, b, m[i+ 2], 17,  606105819);
        b = digest1(b, c, d, a, m[i+ 3], 22, -1044525330);
        a = digest1(a, b, c, d, m[i+ 4],  7, -176418897);
        d = digest1(d, a, b, c, m[i+ 5], 12,  1200080426);
        c = digest1(c, d, a, b, m[i+ 6], 17, -1473231341);
        b = digest1(b, c, d, a, m[i+ 7], 22, -45705983);
        a = digest1(a, b, c, d, m[i+ 8],  7,  1770035416);
        d = digest1(d, a, b, c, m[i+ 9], 12, -1958414417);
        c = digest1(c, d, a, b, m[i+10], 17, -42063);
        b = digest1(b, c, d, a, m[i+11], 22, -1990404162);
        a = digest1(a, b, c, d, m[i+12],  7,  1804603682);
        d = digest1(d, a, b, c, m[i+13], 12, -40341101);
        c = digest1(c, d, a, b, m[i+14], 17, -1502002290);
        b = digest1(b, c, d, a, m[i+15], 22,  1236535329);

        a = digest2(a, b, c, d, m[i+ 1],  5, -165796510);
        d = digest2(d, a, b, c, m[i+ 6],  9, -1069501632);
        c = digest2(c, d, a, b, m[i+11], 14,  643717713);
        b = digest2(b, c, d, a, m[i+ 0], 20, -373897302);
        a = digest2(a, b, c, d, m[i+ 5],  5, -701558691);
        d = digest2(d, a, b, c, m[i+10],  9,  38016083);
        c = digest2(c, d, a, b, m[i+15], 14, -660478335);
        b = digest2(b, c, d, a, m[i+ 4], 20, -405537848);
        a = digest2(a, b, c, d, m[i+ 9],  5,  568446438);
        d = digest2(d, a, b, c, m[i+14],  9, -1019803690);
        c = digest2(c, d, a, b, m[i+ 3], 14, -187363961);
        b = digest2(b, c, d, a, m[i+ 8], 20,  1163531501);
        a = digest2(a, b, c, d, m[i+13],  5, -1444681467);
        d = digest2(d, a, b, c, m[i+ 2],  9, -51403784);
        c = digest2(c, d, a, b, m[i+ 7], 14,  1735328473);
        b = digest2(b, c, d, a, m[i+12], 20, -1926607734);

        a = digest3(a, b, c, d, m[i+ 5],  4, -378558);
        d = digest3(d, a, b, c, m[i+ 8], 11, -2022574463);
        c = digest3(c, d, a, b, m[i+11], 16,  1839030562);
        b = digest3(b, c, d, a, m[i+14], 23, -35309556);
        a = digest3(a, b, c, d, m[i+ 1],  4, -1530992060);
        d = digest3(d, a, b, c, m[i+ 4], 11,  1272893353);
        c = digest3(c, d, a, b, m[i+ 7], 16, -155497632);
        b = digest3(b, c, d, a, m[i+10], 23, -1094730640);
        a = digest3(a, b, c, d, m[i+13],  4,  681279174);
        d = digest3(d, a, b, c, m[i+ 0], 11, -358537222);
        c = digest3(c, d, a, b, m[i+ 3], 16, -722521979);
        b = digest3(b, c, d, a, m[i+ 6], 23,  76029189);
        a = digest3(a, b, c, d, m[i+ 9],  4, -640364487);
        d = digest3(d, a, b, c, m[i+12], 11, -421815835);
        c = digest3(c, d, a, b, m[i+15], 16,  530742520);
        b = digest3(b, c, d, a, m[i+ 2], 23, -995338651);

        a = digest4(a, b, c, d, m[i+ 0],  6, -198630844);
        d = digest4(d, a, b, c, m[i+ 7], 10,  1126891415);
        c = digest4(c, d, a, b, m[i+14], 15, -1416354905);
        b = digest4(b, c, d, a, m[i+ 5], 21, -57434055);
        a = digest4(a, b, c, d, m[i+12],  6,  1700485571);
        d = digest4(d, a, b, c, m[i+ 3], 10, -1894986606);
        c = digest4(c, d, a, b, m[i+10], 15, -1051523);
        b = digest4(b, c, d, a, m[i+ 1], 21, -2054922799);
        a = digest4(a, b, c, d, m[i+ 8],  6,  1873313359);
        d = digest4(d, a, b, c, m[i+15], 10, -30611744);
        c = digest4(c, d, a, b, m[i+ 6], 15, -1560198380);
        b = digest4(b, c, d, a, m[i+13], 21,  1309151649);
        a = digest4(a, b, c, d, m[i+ 4],  6, -145523070);
        d = digest4(d, a, b, c, m[i+11], 10, -1120210379);
        c = digest4(c, d, a, b, m[i+ 2], 15,  718787259);
        b = digest4(b, c, d, a, m[i+ 9], 21, -343485551);

        a = (a + a0) | 0;
        b = (b + b0) | 0;
        c = (c + c0) | 0;
        d = (d + d0) | 0;
      }

      return wordsSwapEndian([a,b,c,d]);
    };

    return function( input ) {
      return bytesToHex(wordsToBytes(md5digest(input)));
    };
  })();
}

function _hash_md5( s ) {
  var res = md5algo(s);
  //console.log("md5(" + s + ") = " + res );
  return res;
}
