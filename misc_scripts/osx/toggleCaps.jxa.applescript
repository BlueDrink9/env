// Run with `osascript -l JavaScript toggleCaps.jxa.applescript`
// vim: ft=javascript
// https://apple.stackexchange.com/a/361402
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
    //  This used to toggle, but for some reason state is always false on 10.11.6 upwards
    $.IOHIDSetModifierLockState(ioConnect, $.kIOHIDCapsLockState, !state[0]);
    $.IOServiceClose(ioConnect);
})();
