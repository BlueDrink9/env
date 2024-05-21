function forEachWidgetInContainmentList(containmentList, callback) {
    for (var containmentIndex = 0; containmentIndex < containmentList.length; containmentIndex++) {
        var containment = containmentList[containmentIndex];

        var widgets = containment.widgets();
        for (var widgetIndex = 0; widgetIndex < widgets.length; widgetIndex++) {
            var widget = widgets[widgetIndex];
            callback(widget, containment);
            if (widget.type === "org.kde.plasma.systemtray") {
                let systemtrayId = widget.readConfig("SystrayContainmentId");
                if (systemtrayId) {
                    forEachWidgetInContainmentList([desktopById(systemtrayId)], callback)
                }
            }
        }
    }
}

function forEachWidget(callback) {
    forEachWidgetInContainmentList(desktops(), callback);
    forEachWidgetInContainmentList(panels(), callback);
}

function forEachWidgetByType(type, callback) {
    forEachWidget(function(widget, containment) {
        if (widget.type == type) {
            callback(widget, containment);
        }
    });
}


function forEachWidgetInContainment(containment, callback) {
   for (var i = 0; i < containment.widgetIds.length; i++) {
            callback(containment.widgetById(containment.widgetIds[i]).type)
        }
}

function removeItemOnce(arr, value) {
  var index = arr.indexOf(value);
  if (index > -1) {
    arr.splice(index, 1);
  }
  return arr;
}



var allPanels = panels();
var done = false;
for (var panelIndex = 0; panelIndex < allPanels.length; panelIndex++) {
    if (done){
        break;
    }
    var p = allPanels[panelIndex];

    var widgets = p.widgets();
    for (var widgetIndex = 0; widgetIndex < widgets.length; widgetIndex++) {
        var w = widgets[widgetIndex];
        if (w.type === "org.kde.plasma.taskmanager"
            || w.type === "org.kde.plasma.icontasks"){
            w.remove();
            continue
        }

        // Whichever panel has the pager; that's the one I want to add the window title widget to.
        // Skip all others.
        if (w.type != "org.kde.plasma.pager"){
            continue;
        }
        done = true;
        var spacer1 = p.addWidget("org.kde.plasma.marginsseparator");
        var wt = p.addWidget("org.kde.windowtitle");


        // Reorder
        p.currentConfigGroup = ["General"];
        var key = "AppletOrder"
        p.reloadConfig();


        var order = p.readConfig(key).split(";");

        // After application menu and pager (both there by default)
        const position_in_panel = 2;

        // Remove the newly created numbers
        order = removeItemOnce(order, wt.id);
        order = removeItemOnce(order, spacer1.id);


        // Insert the IDs
        order.splice(position_in_panel, 0, spacer1.id, wt.id);
        order = order.join(";");
        p.writeConfig(key, order);

        // The new config doesn't get the new applets until a reload is
        // triggered. But it works if we do it from script instead!
        // Build write command to update from command line
        var command=`kwriteconfig5 --file "$(basename "$applet_config")" \
          --group Containments --group "${p.id}" --group General \
          --key "AppletOrder" "${order}"`

        // Print to echo it to bash
        print(command)

        // For debugging
        // wt.remove();
        // spacer1.remove();
        // -debugging
        break;
    }
}
