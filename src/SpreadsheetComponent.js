import React from "react";
import ReactDOM from "react-dom";
import { SpreadSheetReactComponent } from './App.fsproj';
import reactToWebComponent from "react-to-webcomponent";

console.log({SpreadSheetReactComponent});
const SpreadsheetWebComponent = reactToWebComponent(SpreadSheetReactComponent, React, ReactDOM);

customElements.define('spreadsheet-component', SpreadsheetWebComponent);