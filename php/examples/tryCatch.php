<?php

function tc()
{
    try {
        throw new Exception();
    } catch (Exception|ErrorException) {

    }
}

tc();
