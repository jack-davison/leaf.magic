/* global LeafletWidget, L */
// An icon legend control which, like leaflet's own addLegend(), can be shown
// and hidden alongside a layer group.
LeafletWidget.methods.addIconLegend = function (options) {
  var map = this;
  var legend = L.control({ position: options.position });
  // as leaflet's addControl(), only a user-supplied layerId is used as the id
  var divId = options.layerId;

  legend.onAdd = function () {
    var div = L.DomUtil.create("div", options.className);
    if (divId !== undefined && divId !== null) {
      div.setAttribute("id", divId);
    }
    div.innerHTML = options.html;
    return div;
  };

  if (options.group) {
    if (!options.layerId) {
      options.layerId = L.Util.stamp(legend);
    }
    var show = function (e) {
      if (e.name === options.group) {
        map.controls.add(legend, options.layerId);
      }
    };
    var hide = function (e) {
      if (e.name === options.group) {
        map.controls.remove(options.layerId);
      }
    };
    map.on("overlayadd", show);
    map.on("overlayremove", hide);
    map.on("groupadd", show);
    map.on("groupremove", hide);
  }

  map.controls.add(legend, options.layerId);
};
