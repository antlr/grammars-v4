package test_controller

import (
        "aa.bb.com/facility/assert"
        "aa.bb.com/xxx/post/internal/controller"
        "aa.bb.com/xxx/post/pkg/cerror"
        ctx2 "aa.bb.com/xxx/post/test/tools/ctx"
        "aa.bb.com/xxx/post/thrift_gen/base"
        "aa.bb.com/xxx/post/thrift_gen/something/xxx/post"
        "aa.bb.com/gopkg/logs"
)

func newUpdateCommentRequest() *post.UpdateCommentRequest {
        req := post.NewUpdateCommentRequest()
        req.CommentId = 18
        fields := map[string]interface{}{
                "extra": `{"test_update_fields":"update_fields_by_test"}`,
        }
        req.UpdateFields, _ = json.MarshalToString(fields)
        req.Base = &base.Base{
                Caller: "some.interface.withip",
        }
        return req
}

func TestUpdateComment() {
        ctx := ctx2.MockKiteContext("contextname", "")
        req := newUpdateCommentRequest()
        resp, err := controller.UpdateComment(ctx, req)
        logs.Info("%+v %+v %+v", req, resp, err)
        assert.IsNil(err)
        assert.EqualInt32(resp.BaseResp.StatusCode, cerror.ErrNo_Success.Int32())
}

