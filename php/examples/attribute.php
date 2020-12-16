<?php

class PostsController
{
    #[Route("/api/posts/id", methods: ["GET"])]
    public function get($id) { /* ... */ }
}