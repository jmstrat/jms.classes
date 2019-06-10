function _addNewWidget (el, name, x, y, w, h, auto) {
    var grid = $(el).data('gridstack')
    node = $(`<div class="grid-stack-item"><div class="grid-stack-item-content"><span>${name}</span></div></div>`)
    grid.addWidget(node, x, y, w, h, auto);
}

function addNewWidget (el, name) {
    if (typeof name === "undefined") {
      name = $(el).children().length+1
    }
    _addNewWidget(el, name, 0, 0, 1, 1, true)
}
function removeWidget (el) {
    var grid = $(el).data('gridstack')
    var n = $(el).find('.grid-stack-item:visible').length
    var min = $(el).data("min-plots")
    if (n > parseInt(min)) {
      node = $(el).children().last()
      grid.removeWidget(node);
    }
}

function addArrays(ar1, ar2){
    var ar3 = [];
    for (var i in ar1)
        ar3.push(ar1[i] + ar2[i]);
    return ar3;
}

function seq(x0, x1) {
  var result = [];
  for (var i = x0; i <= x1; ++i) result.push(i);
  return (result);
}

function expandGrid() {
    var r = [], arg = arguments, max = arg.length-1;
    function helper(arr, i) {
        for (var j=0, l=arg[i].length; j<l; j++) {
            var a = arr.slice(0); // clone arr
            a.push(arg[i][j]);
            if (i==max)
                r.push(a);
            else
                helper(a, i+1);
        }
    }
    helper([], 0);
    return r;
}

var layoutBinding = new Shiny.InputBinding();
$.extend(layoutBinding, {
  find: function(scope) {
    return $(scope).find(".layout-input");
  },
  initialize: function(el){
    var grid = $(el).find('.grid-stack')
    var options = {
      removable: false,
      verticalMargin: 0,
      disableOneColumnMode: true,
      minWidth: 500,
      width: 10,
      height: 10
    };
    $(grid).gridstack(options);
    var names = $(grid).data("plot-names");
    if (typeof names === 'undefined') {
      names = [];
    }
    $(el).find('#add-button').click(function () {addNewWidget(grid, names[$(grid).children().length])});
    $(el).find('#remove-button').click(function () {removeWidget(grid)});

    var default_layout = $(grid).data("default-layout");
    if (typeof default_layout === 'undefined') {
      names = [];
    }
    var plot, plot_name;
    for(var i=0; i<default_layout.length; i++) {
      plot = default_layout[i];
      plot_name = names[i]
      if (typeof plot_name === 'undefined') {
        plot_name = i + 1
      }
      _addNewWidget(grid, plot_name, plot[0], plot[1], plot[2], plot[3], false)
    }
  },
  getValue: function(el) {
    var grid = $(el).find('.grid-stack')
    var x=[], y=[], w=[], h=[], node, child, input=[];
    var children = $(grid).find('.grid-stack-item');
    if (children.length === 0) {
      return ({matrix: [[]], nPlots: 0});
    }
    for (var i=0; i<children.length; i++) {
      child = children[i];
      node = $(child).data('_gridstack_node');
      x.push(node.x);
      y.push(node.y);
      w.push(node.width);
      h.push(node.height);
      input.push([node.x, node.y, node.width, node.height])
    }
    var total_width = Math.max.apply(null,addArrays(x, w));
    var total_height = Math.max.apply(null,addArrays(y, h));

    var matrix = [], xi, yi, g, cell;
    for(var i=0; i<total_width; i++) {
        matrix[i] = new Array(total_height).fill(x.length + 1);
    }
    for (var i=0; i<x.length; i++) {
      xi = seq(x[i], x[i] + w[i] - 1);
      yi = seq(y[i], y[i] + h[i] - 1);
      g = expandGrid(xi, yi);
      g.forEach(function(cell) {
        matrix[cell[0]][cell[1]] = i + 1;
      });
    }
    return ({matrix: matrix, nPlots: x.length, input: input});
  },
  getType: function(el) {
    return "jms.matrix";
  },
  subscribe: function(el, callback) {
    var grid = $(el).find('.grid-stack')
    $(grid).on('change.layoutBinding', function(event, items) {
        callback(true);
    });
  },
  unsubscribe: function(el) {
    var grid = $(el).find('.grid-stack')
    $(grid).off(".layoutBinding");
  }
});

Shiny.inputBindings.register(layoutBinding);
