# The DS UDP Server Protocol

## 1) Overview

To be written...

## 2) Node Registration

```
Node                            Directory Server                            Node
  ------------ ds-register ------------>
  <--------- node-registered -----------
```

### 2.1) Message: *ds-register* (74 bytes)

Direction: `node -> directory server`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                            Node Id                                            |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x00         |                              Message ID                               |
|     32|  256|
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|                                    Random Bytes (42 bytes)
|     52|  416|
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 2.2) Message: *node-registered*  (74 bytes)

Direction: `directory server -> node`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                            Node Id                                            |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x00         |                              Message ID                               |
|     32|  256|
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|                                    Random Bytes (42 bytes)
|     52|  416|
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

## 3) Node Keepalive

```
Node                            Directory Server                            Node
  ----------- ds-keepalive ------------>
  ----------- ds-keepalive ------------>
  ----------- ds-keepalive ------------>
...
```

### 3.1) Message: *ds-keepalive* (74 bytes)

Direction: `node -> [directory server]`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                            Node Id                                            |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x01         |
|     32|  256|
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|                                    Random Bytes (45 bytes)
|     52|  416|
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

## 4) Tunnel Establishment

```
Node                            Directory Server                            Node
  -------- ds-establish-tunnel -------->
                                        ------- node-establish-tunnel ------->
                                        <------- ds-tunnel-established -------
  <------ node-tunnel-established ------
```

### 4.1) Message: *ds-establish-tunnel* (74 bytes)

Direction:  `node -> directory server`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                          Src Node Id                                          |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x02         |                              Message ID                               |
|     32|  256|                                          Dest Node ID                                         |
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|
|     52|  416|                                    Random Bytes (38 bytes)
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 4.2) Message: *node-establish-tunnel* (74 bytes)

Direction: `directory server -> node`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                      Directory Server ID                                      |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x01         |                              Message ID                               |
|     32|  256|                                          Src Node ID                                          |
|     36|  288|                                         Src IP Address                                        |
|     40|  320|                Src Port Number                |
|     44|  352|
|     48|  384|
|     52|  416|
|     56|  448|                                      Shared Key (32 bytes)
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 4.3) Message: *ds-tunnel-established* (74 bytes)

Direction: `node -> directory server`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                          Dest Node Id                                         |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x03         |                              Message ID                               |
|     32|  256|                                          Src Node ID                                          |
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|
|     52|  416|                                     Random Bytes (38 bytes)
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 4.4) Message: *node-tunnel-established* (74 bytes)

Direction: `directory server -> node`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                      Directory Server ID                                      |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x02         |                               Message ID                              |
|     32|  256|                                          Dest Node ID                                         |
|     36|  288|                                        Dest IP Address                                        |
|     40|  320|                Dest Port Number               |
|     44|  352|
|     48|  384|
|     52|  416|
|     56|  448|                                      Shared Key (32 bytes)
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

## 5) Get Neighbours (Experimental)

```
Node                            Directory Server                            Node
  <-------- node-get-neighbours --------
  ----------- ds-neighbours ----------->
  ----------- ds-neighbours ----------->
  ----------- ds-neighbours ----------->
  ...
```

### 5.1) Message: *node-get-neighbours* (74 bytes)

Direction: `directory server -> node`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                      Directory Server ID                                      |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x03         |                              Message ID                               |
|     32|  256|
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|
|     52|  416|                                    Random Bytes (42 bytes)
|     56|  448|
|     60|  480|
|     64|  512|
|     68|  544|
|     72|  576|                                               |
```

### 5.2) Message: *ds-neighbours* (< 512 bytes)

Direction: `node -> directory server`

```
|Offsets|Octet|           0           |           1           |           2           |           3           |
|  Octet|  Bit|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|
|      0|    0|                                      Directory Server ID                                      |
|      4|   32|
|      8|   64|
|     12|   96|                                        Nonce (24 bytes)
|     16|  128|
|     20|  160|
|     24|  192|                                                                                               |
|     28|  224|          0x04         |                              Message ID                               |
|     32|  256|                Fragment Counter               |                 Fragment Size                 |
|     36|  288|
|     40|  320|
|     44|  352|
|     48|  384|
|     52|  416|
|     56|  448|                                 Fragment (Fragment Size bytes)
|     60|  480|                     (If Fragment Size < 480 bytes it is the last fragment)
|     64|  512|
|     68|  544|
|     72|  576|
...
|     508|4064|                                                                                              |
```

## 5) Get Route Entries (Experimental)

```
Node                            Directory Server                            Node
  <------- node-get-route-entries ------
  --------- ds-route-entries ---------->
  --------- ds-route-entries ---------->
  --------- ds-route-entries ---------->
  ...
```

Same as the *node-get-neighbours* and *ds-route-entries* messages but
with the message types `0x04` and `0x05` respectively.
