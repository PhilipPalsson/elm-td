import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

localforage.getItem('save', function (err, value) {
    const app = Elm.Main.init({
      node: document.getElementById('root'),
      flags: {...value, seed: Math.floor(Math.random() * 10000000)},
    });

    app.ports.saveState.subscribe(function(state) {
        localforage.setItem('save', {gameState: state, timestamp: new Date().toLocaleString()});
    });
});


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
