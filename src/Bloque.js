import React from 'react';
import { numberToColor } from './util';

function Bloque({ value, className }) {
    // value === 0 means the square is empty.    
    return (
        <div
            className={"bloque" + (className ? " " + className : "")}
            style={value === 0 ? undefined : { backgroundColor: numberToColor(value) }}
        >
            {value === 0 ? "" : value}
        </div>
    );
}

export default Bloque;