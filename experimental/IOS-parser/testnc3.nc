
(if ( inPort = 1 && !(dstIP / 10.1.1.0 0.0.0.8 || dstIP / 10.1.2.0 0.0.0.8)) then fwd(2) else drop)
+
(if (!(dstIP / 10.1.1.0 0.0.0.8 || dstIP / 10.1.2.0 0.0.0.8) && inPort = 3) then fwd(4) else drop)


