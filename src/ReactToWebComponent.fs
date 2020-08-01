module ReactToWebComponents

open Fable.Core
open Fable.React
open Fable.React.ReactBindings

type private IWebComponent = abstract _Unused: unit
type private ICustomElementsRegistry =
    abstract define: string * IWebComponent -> unit
let [<Global>] private customElements: ICustomElementsRegistry = jsNative

type private ReactToWebComponent = System.Func<obj,IReactExports,IReactDom,IWebComponent>
[<ImportDefault("react-to-webcomponent")>]
let private reactToWebComponent: ReactToWebComponent = jsNative

let register name reactComponent =
    let c = reactToWebComponent.Invoke(reactComponent, React, ReactDom)
    customElements.define(name, c)


