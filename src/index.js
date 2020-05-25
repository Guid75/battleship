import {Elm} from './Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('app')
})

app.ports.requestSvgMousePos.subscribe(requestSvgMousePos);

function requestSvgMousePos(pos) {
  var svg = document.getElementById(pos[0]);
  var rect = svg.getBoundingClientRect();
  var pt = svg.createSVGPoint();
  pt.x = pos[1];
  pt.y = pos[2];
  var p = pt.matrixTransform(svg.getScreenCTM().inverse());
  app.ports.svgMousePosResult.send([pos[0], p.x, p.y]);
}
