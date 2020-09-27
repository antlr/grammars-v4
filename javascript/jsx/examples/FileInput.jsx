import React, { Component } from 'react';
import PropTypes from 'prop-types';
import _ from 'lodash';
import { defineMessages, injectIntl, FormattedMessage } from 'react-intl';

import { jiveCoreJavascriptRequire } from 'mitui-base-client';

const messages = defineMessages({
  editText: {
    id: 'profile.banner.edit_button.edit_text',
    defaultMessage: 'Choose image'
  },
  fileExceedsSizeLimit: {
    id: 'profile.banner.edit_button.file_exceeds_size_limit',
    defaultMessage: 'The file you have chosen exceeds the size limit of {size}MB.'
  },
  filesExceedSizeLimit: {
    id: 'profile.banner.edit_button.files_exceed_size_limit',
    defaultMessage: 'One or more of the files chosen exceed the size limit of {size}MB.'
  }
});

class FileInput extends Component {
  static contextTypes = {
    executeAction: PropTypes.func.isRequired,
  };

  static propTypes = {
    accept    : PropTypes.string,
    action    : PropTypes.string.isRequired,
    className : PropTypes.string,
    fileName  : PropTypes.string,
    maxFileSize: PropTypes.number,
    multiple  : PropTypes.bool,
    onBeforeUpload   : PropTypes.func,
    onUploadSuccess  : PropTypes.func.isRequired,
    text      : PropTypes.string,
    tokenName : PropTypes.string.isRequired
  };

  static defaultProps = {
    className : '',
    fileName  : 'file',
    maxFileSize: Infinity,
    multiple  : false,
    onBeforeUpload: () => {},
    text      : ''
  };

  acceptAttribute() {
    switch (this.props.accept) {
      case 'images':
        return 'image/*';

      case 'videos':
        return 'video/*';

      case 'audio':
        return 'audio/*';

      default:
        return this.props.accept;
    }
  }

  componentDidMount() {
    jiveCoreJavascriptRequire.then((requirejs) => {
      requirejs(['apps/shared/models/file_uploader']);
    });
  }

  onChange() {
    const resetForm = () => {
      let inputs = this.refs.form.querySelectorAll('input[type="hidden"]');
      for (let i=0; i < inputs.length; i++) {
        inputs[i].parentNode.removeChild(inputs[i]);
      }
      this.refs.form.reset();
    };
    jiveCoreJavascriptRequire.then((requirejs) => {
      requirejs(['apps/shared/models/file_uploader'], uploader => {
        let files = _.toArray(this.refs.file.files || []);

        if (files.every(file => (file.size <= this.props.maxFileSize))) {
          this.props.onBeforeUpload();
          uploader.uploadForm(this.refs.form, { tokenName: this.props.tokenName }).then(result => {
            resetForm();
            if (result.errorType) {
              this.notifyOfError()
            } else {
              this.props.onUploadSuccess(result);
            }
          }, err => {
            console.error(err);
            resetForm();
          });
        } else {
          resetForm();
          this.notifyOfError();
        }
      });
    });
  }

  notifyOfError() {
    let { formatMessage } = this.props.intl;
    let size = (this.props.maxFileSize / 1024) / 1000;
    let msg = this.props.multiple ? formatMessage(messages.filesExceedSizeLimit, { size }) : formatMessage(messages.fileExceedsSizeLimit, { size });

    this.context.executeAction(actionContext => actionContext.dispatch('DISPLAY_MESSAGE', {level: 'warn', message: msg}));
  }

  render() {
    let attributes = { type: 'file', name: this.props.fileName, id: this.props.fileName };
    attributes.onChange = this.onChange.bind(this);
    if (this.props.accept) {
      attributes.accept = this.acceptAttribute();
    }
    if (this.props.multiple) {
      attributes.multiple = true;
    }

    return (
      <form ref="form" action={this.props.action} method="post" encType="multipart/form-data">
        <label htmlFor={attributes.id} className={this.props.className}>
          <FormattedMessage {...messages.editText}/>
        </label>
        <input ref="file" { ...attributes } />
      </form>
    );
  }
}

export { FileInput as _FileInput };
export default injectIntl(FileInput);
