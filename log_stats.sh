#!/bin/bash

PKT_INS=$(cat log/debug.log | grep "\[pkt_in]" | wc -l)
PKT_OUTS=$(cat log/debug.log | grep "\[pkt_out]" | wc -l)
FLOW_MODS=$(cat log/debug.log | grep "\[flow]" | wc -l)

echo -e "packet_in count: $PKT_INS
packet_out count: $PKT_OUTS
flow_mod count: $FLOW_MODS"
