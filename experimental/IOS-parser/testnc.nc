(filter ( !(dstIP / 10.1.1.0 0.0.0.8 || dstIP / 10.1.2.0 0.0.0.8) && inPort = 1); fwd(2))
+
(filter ( !(dstIP / 10.1.1.0 0.0.0.8 || dstIP / 10.1.2.0 0.0.0.8) && inPort = 3); fwd(4))