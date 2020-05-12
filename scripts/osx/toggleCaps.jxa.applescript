// Run with `osascript -l JavaScript toggleCaps.jxa.applescript`
ObjC.import("IOKit");
ObjC.import("CoreServices");


(() => {
    var ioConnect = Ref();
    var state = Ref();

    $.IOServiceOpen(
        $.IOServiceGetMatchingService(
            $.kIOMasterPortDefault,
            $.IOServiceMatching(
                $.kIOHIDSystemClass
            )
        ),
        $.mach_task_self_,
        $.kIOHIDParamConnectType,
        ioConnect
    );
    $.IOHIDGetModifierLockState(ioConnect, $.kIOHIDCapsLockState, state);
    $.IOHIDSetModifierLockState(ioConnect, $.kIOHIDCapsLockState, !state[0]);
    $.IOServiceClose(ioConnect);
})();
