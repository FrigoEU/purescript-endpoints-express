// module EndpointExample.Client
/* global document, exports */

exports.appendToBody = function(str){
  return function(){
    var div = document.createElement("div");
    div.innerHTML = str;
    document.body.appendChild(div);
  };
};
