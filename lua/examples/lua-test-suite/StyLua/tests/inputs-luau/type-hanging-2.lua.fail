-- https://github.com/JohnnyMorganz/StyLua/issues/372
export type Visitor<KindToNode, Nodes = any> =
       EnterLeave<
               VisitFn<Nodes>
               | ShapeMap<KindToNode, <Node>(Node) -> VisitFn<Nodes, Node>>
       >
       | ShapeMap<
               KindToNode,
               <Node>(Node) -> VisitFn<Nodes, Node> | EnterLeave<VisitFn<Nodes, Node>>>
