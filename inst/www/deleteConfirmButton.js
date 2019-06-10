var deleteConfirmButtonInputBinding = new Shiny.InputBinding();
$.extend(deleteConfirmButtonInputBinding, {
  find: function find(scope) {
    return $(scope).find(".delete-confirm-button");
  },
  getValue: function getValue(el) {
    return $(el).data('val') || 0;
  },
  setValue: function setValue(el, value) {
    $(el).data('val', value);
  },
  getType: function getType(el) {
    return 'shiny.action';
  },
  subscribe: function subscribe(el, callback) {
    $(el).on("click.deleteConfirmButtonInputBinding", function (e) {
      if($(this).hasClass("confirm")){
        $(this).removeClass("confirm");
        $(this).addClass("done");
        $(this).find('span').text("Deleted");
        var val = $(this).data('val') || 0;
        $(this).data('val', val + 1);
        callback();
      } else if($(this).hasClass("done")){
        $(this).removeClass("done");
        $(this).find('span').text("Delete");
      } else {
        $(this).addClass("confirm");
        $(this).find('span').text("Are you sure?");
      }
    });

    $(el).on("mouseout.deleteConfirmButtonInputBinding", function() {
      if($(this).hasClass("confirm") || $(this).hasClass("done")){
        var $el = $(this);
        setTimeout(function() {
          $el.removeClass("confirm").removeClass("done");
          $el.find('span').text("Delete");
        }, 2000);
      }
    });
  },
  getState: function getState(el) {
    return { value: this.getValue(el) };
  },
  unsubscribe: function unsubscribe(el) {
    $(el).off(".deleteConfirmButtonInputBinding");
    $(el).off("mouseout");
  }
});
Shiny.inputBindings.register(deleteConfirmButtonInputBinding);

$(document).on('click', 'a.delete-confirm-button', function (e) {
  e.preventDefault();
});
