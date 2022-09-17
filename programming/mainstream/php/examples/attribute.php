<?php

class PostsController
{
    #[Route("/api/posts/id", methods: ["GET"])]
    #[Attr2]
    public function get($id) { /* ... */ }
}