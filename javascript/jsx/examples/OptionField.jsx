import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { findDOMNode } from 'react-dom'
import { JivePopover, AutopositionOverlay } from 'mitui-util-bootstrap';

class OptionField extends Component {
  static propTypes = {
    id         : PropTypes.number.isRequired,
    onComplete : PropTypes.func.isRequired,
    optionId   : PropTypes.number,
    options    : PropTypes.arrayOf(PropTypes.object).isRequired,
    title      : PropTypes.string.isRequired,
    value      : PropTypes.string
  };

  static defaultProps = {
    optionId : -1,
    value    : ''
  };

  state = {
    showPopover: false
  }

  hide() {
    this.setState({ showPopover: false });
  }

  onSelectOption(option, e) {
    e.preventDefault();
    this.props.onComplete(option);
  }

  handlePopoverToggleClick(e) {
    e.preventDefault();
    this.setState({ showPopover: !this.state.showPopover });
  }

  render() {
    return (
      <span>
        <a ref="popoverToggle" href="#" onClick={ this.handlePopoverToggleClick.bind(this) }>
          { this.props.value || this.props.title }
        </a>
        <AutopositionOverlay
          show={ this.state.showPopover }
          target={ () => findDOMNode(this.refs.popoverToggle) }
          placement="bottom"
          animation={ false }
          onHide={ () => this.setState({ showPopover: false })}
          rootClose={ true }>
          <JivePopover id={`option-field-${this.props.id}`} className="j2-pages-mitui-profile-header-popover-availability">
            <h4>{this.props.title}</h4>

            <ul className="j2-list-std">
              {this.props.options.map((option, i) => (
                <li key={`options-field-item-${i}`}>
                  <a href="#" onClick={this.onSelectOption.bind(this, option)}>
                    {option.value}
                  </a>
                </li>
                ))}
              </ul>
            </JivePopover>
          </AutopositionOverlay>
      </span>
    );
  }
}


export default OptionField;
