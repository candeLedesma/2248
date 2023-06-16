import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult} from './util';
import Square from './Square';
let pengine;

function Game() {

  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  const [nuevo, setNuevo] = useState(0);
  const [gravedad, setGravedad] = useState(false); 


  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPath(newPath);
    console.log(JSON.stringify(newPath));
   
  }

  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);

    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
       
      } else {
        setWaiting(false);
      }
    });
  }
  /*booster(Grilla,NumOfColumns,Resultado)*/ 
  function activateBooster() {
    if(path.length === 0 && gravedad === false){
      const gridS = JSON.stringify(grid);
      const queryS = "booster(" + gridS + "," + numOfColumns + ", RGrid)";
      setWaiting(true);
      pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['RGrid']); 
        
      }
      setWaiting(false);
    });
    }

  }


  function activateAyudaMaxima() {
    if(path.length === 0 &&  gravedad === false){
      const gridS = JSON.stringify(grid);
      const queryS = "ayudaMaxima(" + gridS + "," + numOfColumns + ", RCamino, SumaCamino)";
      
      pengine.query(queryS, (success, response) => {
        if (success) {
          setPath(response['RCamino']);
          setNuevo(response['SumaCamino']);
        }
        setWaiting(false);
      });
    }
  }


  function activateAyudaMaximosIguales() {
    if(path.length === 0 && gravedad === false){
      const gridS = JSON.stringify(grid);
      const queryS = "ayudaMaximosIguales(" + gridS + "," + numOfColumns + ", RCamino, SumaCamino)";
      pengine.query(queryS, (success, response) => {
        if (success) {
          setPath(response['RCamino']);
          setNuevo(response['SumaCamino']);
        }
        setWaiting(false);
      });
    }
  }


  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGravedad(true);
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 100);//1000
    } else {
      setWaiting(false);
    }
    setGravedad(false);
  }

  if (grid === null) {
    return null;
  }
 
  return (
    
    <div className="game">
      <div className="header">
        {path.length > 1
          ? <Square value={nuevo} className="last-square" />
          : <div className="score">{score} </div>
        }
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
        setNuevo = {setNuevo}
      />
      <button onClick={activateBooster}>
        Booster
      </button>
      <button onClick={activateAyudaMaxima}>
        MaxHelp
      </button>
      <button onClick={activateAyudaMaximosIguales}>
        MaximosIguales
      </button>
    </div>

  );
}

export default Game;