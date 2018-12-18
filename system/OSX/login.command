#!/bin/bash
# Remap capslock to backspace
hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039, "HIDKeyboardModifierMappingDst":0x70000002A}]}'
